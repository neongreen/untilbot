{-# LANGUAGE
RecordWildCards,
OverloadedStrings,
MultiWayIf,
TemplateHaskell,
FlexibleContexts,
TypeFamilies,
RankNTypes,
NoImplicitPrelude
  #-}


module Main where


-- General
import BasePrelude hiding (threadDelay, catch)
-- Lenses
import Lens.Micro.Platform hiding ((&))
-- Default
import Data.Default.Class
-- Monads and monad transformers
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Maybe
-- Containers
import Data.Map (Map)
import qualified Data.Map as M
-- Text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.Text.Format hiding (format, print)
import qualified Data.Text.Format as Format (format)
import Data.Text.Format.Params (Params)
-- Parsing
import Text.Megaparsec hiding (Message)
import Text.Megaparsec.Text
import Text.Megaparsec.Lexer (integer)
-- acid-state
import Data.Acid as Acid
import Data.SafeCopy
-- Concurrency
import Control.Concurrent.Lifted
-- Exceptions
import Control.Exception.Lifted
-- Time
import Data.Time
-- Telegram
import Telegram


data Goal = Goal {
  goalStart    :: UTCTime,
  goalEnd      :: UTCTime,
  goalLength   :: NominalDiffTime,
  originalChat :: ChatId,
  goalText     :: Text }
  deriving (Show)

deriveSafeCopy 0 'base ''Goal

data Status = Resting | Doing Goal | Judging Goal
  deriving (Show)

deriveSafeCopy 0 'base ''Status

data GoalResult
  = CompletedEarly NominalDiffTime
  | Completed
  | Failed
  deriving (Show)

deriveSafeCopy 0 'base ''GoalResult

data UserData = UserData {
  _status        :: Status,
  _archivedGoals :: [(Goal, GoalResult)],
  _dayEnd        :: TimeOfDay,
  _timeZone      :: TimeZone }
  deriving (Show)

deriveSafeCopy 0 'base ''UserData

makeLenses ''UserData

instance Default UserData where
  def = UserData {
    _status        = Resting,
    _archivedGoals = [],
    _dayEnd        = TimeOfDay 6 0 0,  -- 6am
    _timeZone      = utc }

data BotState = BotState {
  _userData :: Map UserId UserData }

deriveSafeCopy 0 'base ''BotState

makeLenses ''BotState

instance Default BotState where
  def = BotState {
    _userData = mempty }

archiveGoal :: UserId -> GoalResult -> Acid.Update BotState ()
archiveGoal user res = do
  mbStatus <- gets (preview (userData.(ix user).status))
  userData.(ix user).status .= Resting
  case mbStatus of
    Just (Doing   goal) -> userData.(ix user).archivedGoals %= ((goal, res) :)
    Just (Judging goal) -> userData.(ix user).archivedGoals %= ((goal, res) :)
    Just Resting        -> return ()
    Nothing             -> return ()

setGoal :: UserId -> Goal -> Acid.Update BotState ()
setGoal user goal = do
  userData.(ix user).status .= Doing goal

setStatus :: UserId -> Status -> Acid.Update BotState ()
setStatus user st = do
  userData.(ix user).status .= st

userPresent :: UserId -> Acid.Query BotState Bool
userPresent user = has (userData.(ix user)) <$> ask

addUser :: UserId -> UserData -> Acid.Update BotState ()
addUser user dt = userData.(at user) .= Just dt

removeUser :: UserId -> Acid.Update BotState ()
removeUser user = userData.(at user) .= Nothing

setDayEnd :: UserId -> TimeOfDay -> Acid.Update BotState ()
setDayEnd user t = userData.(ix user).dayEnd .= t

setTimeZone :: UserId -> TimeZone -> Acid.Update BotState ()
setTimeZone user tz = userData.(ix user).timeZone .= tz

getUserData :: UserId -> Acid.Query BotState (Maybe UserData)
getUserData user = view (userData.(at user))

getAllUserData :: Acid.Query BotState (Map UserId UserData)
getAllUserData = view userData

makeAcidic ''BotState [
  'archiveGoal, 'setGoal, 'setStatus,
  'userPresent, 'addUser, 'removeUser,
  'setDayEnd, 'setTimeZone,
  'getUserData, 'getAllUserData ]

type DB = AcidState BotState

main :: IO ()
main = void $ do
  botToken <- T.readFile "telegram-token"
  db <- openLocalStateFrom "state/" def
  createArchive db
  runTelegram botToken (bot db)
  closeAcidState db

bot :: DB -> Telegram ()
bot db = do
  -- Run the goal-checking loop.
  fork $ forever $ ignoreErrors $ ignoreExceptions $ do
    checkGoals db
    threadDelay 1000000
  -- Run the loop that accepts incoming messages.
  forever $ ignoreErrors $ ignoreExceptions $ do
    updates <- getUpdates
    for_ updates $ \Update{..} ->
      processMessage db message

-- Check every goal and send messages about ones that have ended.
checkGoals :: DB -> Telegram ()
checkGoals db = do
  currentTime <- liftIO $ getCurrentTime
  allUserData <- liftIO $ query db GetAllUserData
  for_ (M.toList allUserData) $ \(user, UserData{..}) ->
    case _status of
      Doing goal | goalEnd goal < currentTime -> do
        liftIO $ update db (SetStatus user (Judging goal))
        sendMessage_ (originalChat goal) (quote (goalText goal))
      _ -> return ()

processMessage :: DB -> Message -> Telegram ()
processMessage db message = void $ runMaybeT $ do
  -- Both user and message text have to be present (otherwise we don't do
  -- anything).
  -- 
  -- from message :: Maybe User
  -- text message :: Maybe Text
  -- 
  -- Read about MaybeT if you don't understand how this works.
  user <- liftMaybe (from message)
  let uid = userId user
  text <- liftMaybe (text message)

  let update_ :: (MonadIO m, EventState event ~ BotState, UpdateEvent event)
              => event -> m (EventResult event)
      update_ x = liftIO $ update db x
      query_ :: (MonadIO m, EventState event ~ BotState, QueryEvent event)
             => event -> m (EventResult event)
      query_ x = liftIO $ query db x

  -- If we haven't seen this user yet, add them to the database and print help
  -- (because it's a new user).
  newUser <- not <$> query_ (UserPresent uid)
  when newUser $ do
    update_ (AddUser uid def)
    lift $ respond_ message helpText

  Just UserData{..} <- query_ (GetUserData uid)

  currentTime <- liftIO $ getCurrentTime

  let startFromNow :: Goal -> Goal
      startFromNow goal = goal {
        goalStart = currentTime,
        goalEnd   = addUTCTime (goalLength goal) currentTime }

  lift $ case text of
    -- “?” means “query status”
    "?" -> do
      statusText <- liftIO $ showStatus _status
      respond_ message statusText

    "done" -> case _status of
      Resting ->
        respond_ message "but you weren't doing anything"
      Doing goal -> do
        let leftTime = diffUTCTime (goalEnd goal) (date message)
        if leftTime <= 0
          then update_ (ArchiveGoal uid Completed)
          else do
            update_ (ArchiveGoal uid (CompletedEarly leftTime))
            respond_ message $ format "okay, you finished {} early"
                                      [showDuration leftTime]
      Judging _ ->
        update_ (ArchiveGoal uid Completed)

    "fail" -> case _status of
      Resting ->
        respond_ message "but you weren't doing anything"
      Doing _ -> do
        update_ (ArchiveGoal uid Failed)
        respond_ message "okay"
      Judging _ ->
        update_ (ArchiveGoal uid Failed)

    "again" -> case _status of
      Doing _ ->
        respond_ message "you haven't finished the previous goal"
      Judging _ ->
        respond_ message "you still haven't said anything about \
                         \the goal you want to repeat; say “done, again” \
                         \or “fail, again”"
      Resting ->
        case _archivedGoals ^? _head of
          Nothing ->
            respond_ message "can't do anything “again” since you \
                             \haven't ever scheduled any goals"
          Just (goal, _) -> do
            update_ (SetGoal uid (startFromNow goal))
            respond_ message $ quote (goalText goal) <>
                               blankline <>
                               "repeating this"

    "done, again" -> case _status of
      Resting ->
        respond_ message "but you weren't doing anything"
      Doing goal -> do
        let leftTime = diffUTCTime (goalEnd goal) (date message)
        if leftTime <= 0
          then update_ (ArchiveGoal uid Completed)
          else do
            update_ (ArchiveGoal uid (CompletedEarly leftTime))
            respond_ message $ format "okay, you finished {} early"
                                      [showDuration leftTime]
        update_ (SetGoal uid (startFromNow goal))
      Judging goal -> do
        update_ (ArchiveGoal uid Completed)
        update_ (SetGoal uid (startFromNow goal))

    "fail, again" -> case _status of
      Resting ->
        respond_ message "but you weren't doing anything"
      Doing _ ->
        -- TODO: allow fail&again
        respond_ message "ignored (use reset instead)"
      Judging goal -> do
        update_ (ArchiveGoal uid Failed)
        update_ (SetGoal uid (startFromNow goal))

    "reset" -> case _status of
      Resting ->
        respond_ message "but you weren't doing anything"
      Doing goal -> do
        update_ (SetGoal uid (startFromNow goal))
        respond_ message "okay"
      Judging goal ->
        update_ (SetGoal uid (startFromNow goal))

    "help" -> respond_ message helpText

    "stats" -> do
      let bounds = getDayBounds currentTime _dayEnd _timeZone
      let results = do
            (goal, goalRes) <- _archivedGoals
            Just todayStatus <- return (goalTodayStatus bounds (goal, goalRes))
            return (goal, goalRes, todayStatus)
      respond_ message (showStats (reverse results))

    -- “now is 10.30” or something (needed to figure out the time zone)
    _ | Just (h, m) <- parseNowIs text -> do
      let tz = guessTimeZone currentTime (TimeOfDay h m 0)
      update_ (SetTimeZone uid tz)
      respond_ message (format "okay, stored your timezone ({})" [show tz])

    -- “end of day at 6.00”
    _ | Just (h, m) <- parseEndOfDayAt text -> do
      update_ (SetDayEnd uid (TimeOfDay h m 0))
      respond_ message "okay"

    -- If the message a valid goal that can be parsed, either schedule it or
    -- say that the user already has an active/judged goal
    _ | Just (duration, goalText) <- parseGoal text ->
      case _status of
        Doing   _ -> respond_ message "you already are doing something"
        Judging _ -> respond_ message "you still haven't said anything about \
                                      \the previous goal"
        Resting ->
          update_ $ SetGoal uid Goal {
            goalStart    = currentTime,
            goalEnd      = addUTCTime duration currentTime,
            goalLength   = duration,
            originalChat = chatId (chat message),
            goalText     = goalText }

    "debug forget" -> do
      liftIO $ update db (RemoveUser uid)
      respond_ message "cleared all data about you and forgot you"

    -- Otherwise, tell the user we couldn't parse the command
    _ -> respond_ message "couldn't parse what you said"

  liftIO $ createCheckpoint db

showStatus :: Status -> IO Text
showStatus Resting = return "you're not doing anything"
showStatus (Doing x) = do
  currentTime <- getCurrentTime
  let leftTime = diffUTCTime (goalEnd x) currentTime
  return $ quote (goalText x) <>
           blankline <>
           (showDuration leftTime <> " left")
showStatus (Judging x) =
  return $ quote (goalText x) <>
           blankline <>
           "waiting for you to say something about this"

helpText :: Text
helpText = T.unlines [
  "write something like “10m wash dishes” to set a goal, and in 10 minutes you'll be notified about it; then you can say:",
  "",
  "  • “done” if you have done it",
  "  • “fail” if you haven't",
  "  • “reset” if you forgot about the goal and want to restart it",
  "",
  "you can also say “done” and “reset” while the goal is in progress",
  "",
  "“fail, again” and “done, again” are useful when you want to repeat a goal that just finished; you can also say “again” to repeat the last goal no matter how long ago it was",
  "",
  "——————————",
  "",
  "to find out what you're supposed to be doing now, say “?”",
  "",
  "——————————",
  "",
  "to specify time you can use stuff like “1h45m”, “3m30s”, etc. Russian abbreviations (‘ч’, ‘м’, ‘с’) are supported too",
  "",
  "——————————",
  "",
  "you can say “stats” to see what you've done today; in order for stats to work correctly, say what time it is now (“now is HH.MM”) and say when your day ends (“end of day at HH.MM”; the default is 6am)"
  ]

-- Stats

data GoalTodayStatus = Partly NominalDiffTime | Fully
  deriving (Show)

goalTodayStatus
  :: (UTCTime, UTCTime)
  -> (Goal, GoalResult)
  -> Maybe GoalTodayStatus
goalTodayStatus (start, end) (Goal{..}, result)
  -- ended before the day started, or started after the day ended
  | actualGoalEnd <= start || goalStart >= end = Nothing
  -- started and ended during the day
  | goalStart >= start && actualGoalEnd <= end = Just Fully
  -- otherwise, it intersects with the day somehow
  | otherwise = Just $ Partly $ diffUTCTime (min end actualGoalEnd)
                                            (max start goalStart)
  where
    actualGoalEnd = case result of
      CompletedEarly x -> addUTCTime (-x) goalEnd
      _                -> goalEnd

showStats :: [(Goal, GoalResult, GoalTodayStatus)] -> Text
showStats [] = "today you did nothing"
showStats xs = T.unlines $ concat [
  ["completed goals today:"],
  [""],
  map (("  • " <>) . showGoal) xs,
  [""],
  [format "time spent doing stuff: {}"
          [showDuration (sum (map usefulTime xs))]]]
  where
    usefulTime (Goal{..}, Failed          , _       ) = 0
    usefulTime (Goal{..}, _               , Partly x) = x
    usefulTime (Goal{..}, Completed       , Fully   ) = goalLength
    usefulTime (Goal{..}, CompletedEarly x, Fully   ) = goalLength - x
    --
    showGoal (Goal{..}, Completed, Fully) =
      format "{} – {}" (goalText, showDuration goalLength)
    showGoal (Goal{..}, CompletedEarly x, Fully) =
      format "{} – {}" (goalText, showDuration (goalLength - x))
    showGoal (Goal{..}, Failed, Fully) =
      format "{} – {} (failed)" (goalText, showDuration goalLength)
    showGoal (Goal{..}, Completed, Partly x) =
      format "{} – {} (partly today)" (goalText, showDuration x)
    showGoal (Goal{..}, CompletedEarly _, Partly x) =
      format "{} – {} (partly today)" (goalText, showDuration x)
    showGoal (Goal{..}, Failed, Partly x) =
      format "{} – {} (partly today, failed)" (goalText, showDuration x)

-- Parsing

parseGoal :: Text -> Maybe (NominalDiffTime, Text)
parseGoal = parseMaybe $ do
  duration <- durationP
  skipSome spaceChar
  rest <- many anyChar
  return (duration, T.pack rest)

-- TODO: support fractions
durationP :: Parser NominalDiffTime
durationP = do
  items <- some $ do
    n <- integer
    let strings = choice . map string . words
    choice [
      strings "h hr hrs ч час" *> pure (n*3600),
      strings "m min    м мин" *> pure (n*60),
      strings "s sec    с сек" *> pure n ]
  return (fromInteger (sum items))

hmP :: Parser (Int, Int)
hmP = do
  h <- fromIntegral <$> integer
  char '.' <|> char ':'
  m <- fromIntegral <$> integer
  return (h, m)

parseNowIs :: Text -> Maybe (Int, Int)
parseNowIs = parseMaybe $ do
  string "now is "
  hmP

parseEndOfDayAt :: Text -> Maybe (Int, Int)
parseEndOfDayAt = parseMaybe $ do
  string "end of day at "
  hmP

-- Utils

ignoreErrors :: Telegram a -> Telegram ()
ignoreErrors x = void x `catchError` (liftIO . print)

ignoreExceptions :: Telegram a -> Telegram ()
ignoreExceptions x = void x `catch` \(SomeException a) -> liftIO (print a)

getDayBounds
  :: UTCTime              -- ^ Current time (UTC)
  -> TimeOfDay            -- ^ Start of day
  -> TimeZone             -- ^ Time zone
  -> (UTCTime, UTCTime)   -- ^ (some moment in time that is before now,
                          --    some moment in time that is after now)
getDayBounds currentTime start zone =
  (trueStartOfDay, succDay trueStartOfDay)
  where
    timeThere = utcToLocalTime zone currentTime
    someStartOfDay = localTimeToUTC zone timeThere{localTimeOfDay = start}
    trueStartOfDay =
      if someStartOfDay >= currentTime
        then until (< currentTime) predDay someStartOfDay
        else predDay $ until (>= currentTime) succDay someStartOfDay

predDay :: UTCTime -> UTCTime
predDay t = t { utctDay = pred (utctDay t),
                utctDayTime = min 86400 (utctDayTime t) }

succDay :: UTCTime -> UTCTime
succDay t = t { utctDay = succ (utctDay t),
                utctDayTime = min 86400 (utctDayTime t) }

guessTimeZone
  :: UTCTime    -- ^ Current time
  -> TimeOfDay  -- ^ Time
  -> TimeZone
guessTimeZone currentTime timeThere = minutesToTimeZone diffMinutes''
  where
    timeHere = timeToTimeOfDay (utctDayTime currentTime)
    diffMinutes = (todHour timeThere - todHour timeHere) * 60 +
                  (todMin timeThere - todMin timeHere)
    -- the difference between 2 times must be between −12 and 12h
    -- (UTC+14 actually exists but let's say it's the same as UTC-10)
    diffMinutes'  = diffMinutes `mod` (24*60)
    diffMinutes'' = if diffMinutes' > 12*60 then diffMinutes' - 24*60
                                            else diffMinutes'

showDuration :: NominalDiffTime -> Text
showDuration diff = do
  let (h, rest) = (ceiling diff :: Integer) `divMod` 3600
      (m, s)    = rest `divMod` 60
  if | h == 0 && m == 0 && s == 0 -> "0 seconds"
     | h == 0 && m < 5 ->
         -- being precise
         T.unwords $ [showPlural m "minute" | m /= 0] ++
                     [showPlural s "second" | s /= 0]
     | otherwise ->
         -- rounding
         let m_all :: Double
             m_all = fromIntegral h * 60
                   + fromIntegral m
                   + fromIntegral s / 60
             (h', m') = round m_all `divMod` 60
         in T.unwords $ [showPlural h' "hour"   | h' /= 0] ++
                        [showPlural m' "minute" | m' /= 0]

showPlural :: Integer -> Text -> Text
showPlural n x
  | n `mod` 10 == 1 && n `mod` 100 /= 11 = T.pack (show n) <> " " <> x
  | otherwise                            = T.pack (show n) <> " " <> x <> "s"

-- | A wrapper around 'Format.format' that returns a strict Text.
format :: Params ps => Format -> ps -> Text
format f ps = TL.toStrict (Format.format f ps)

quote :: Text -> Text
quote = T.unlines . map ("> " <>) . T.lines

blankline :: Text
blankline = "\n"

liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return
