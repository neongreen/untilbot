{-# LANGUAGE
RecordWildCards,
OverloadedStrings,
MultiWayIf,
TemplateHaskell,
NoImplicitPrelude
  #-}


module Main where


-- General
import BasePrelude hiding (
  threadDelay,
  catch,
  readMVar, takeMVar, newMVar, putMVar, modifyMVar_ )
-- Lenses
import Lens.Micro.GHC hiding ((&))
import Lens.Micro.TH
-- Default
import Data.Default.Class
-- Monad transformers
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
-- Concurrency and MVars
import Control.Concurrent.Lifted
-- Exceptions
import Control.Exception.Lifted
-- Time
import Data.Time
-- Telegram
import Telegram


data Goal = Goal {
  goalEnd         :: UTCTime,
  goalLength      :: NominalDiffTime,
  originalMessage :: Message,
  goalText        :: Text }
  deriving (Show)

data Status = Resting | Doing Goal | Judging Goal
  deriving (Show)

data GoalResult
  = CompletedEarly NominalDiffTime
  | Completed
  | Failed
  deriving (Show)

data UserData = UserData {
  _userStatus    :: Status,
  _archivedGoals :: [(Goal, GoalResult)] }
  deriving (Show)

makeLenses ''UserData

instance Default UserData where
  def = UserData {
    _userStatus    = Resting,
    _archivedGoals = [] }

main :: IO ()
main = void $ do
  botToken <- T.readFile "telegram-token"
  runTelegram botToken bot

bot :: Telegram ()
bot = do
  -- Create a variable holding user data.
  userDataVar <- newMVar mempty
  -- Run the goal-checking loop.
  fork $ forever $ ignoreErrors $ do
    checkGoals userDataVar
    threadDelay 1000000
  -- Run the loop that accepts incoming messages.
  onUpdateLoop $ \Update{..} ->
    ignoreErrors $ ignoreExceptions $ processMessage userDataVar message

-- Check every goal and send messages about ones that have ended.
checkGoals :: MVar (Map UserId UserData) -> Telegram ()
checkGoals userDataVar = modifyMVar_ userDataVar $ \userData -> do
  currentTime <- liftIO getCurrentTime
  forM userData $ \ud ->
    case ud ^. userStatus of
      Doing goal | goalEnd goal < currentTime -> do
        respond (originalMessage goal) (quote (goalText goal))
        return $ ud & userStatus .~ Judging goal
      _other -> return ud

processMessage :: MVar (Map UserId UserData) -> Message -> Telegram ()
processMessage userDataVar message = void $ runMaybeT $ do
  -- Both user and message text have to be present (otherwise we don't do
  -- anything).
  -- 
  -- from message :: Maybe User
  -- text message :: Maybe Text
  -- 
  -- Read about MaybeT if you don't understand how this works.
  user <- liftMaybe (from message)
  text <- liftMaybe (text message)

  -- If we haven't seen this user yet, create an empty UserData record for
  -- nem. This makes 'setGoal', 'archiveGoal', and getting status simpler,
  -- because we no longer have to care about missing entries.
  -- 
  -- Also print help (because it's a new user).
  newUser <- M.member (userId user) <$> readMVar userDataVar
  when newUser $ do
    lift $ respond_ message helpText
    modifyMVar_ userDataVar $ \userData -> return $
      M.insert (userId user) def userData

  thisUserData <- (M.! userId user) <$> readMVar userDataVar
  let status = thisUserData ^. userStatus

  currentTime <- liftIO $ getCurrentTime

  let setGoal :: Goal -> Telegram ()
      setGoal goal = modifyMVar_ userDataVar $ \userData -> return $
        userData & ix (userId user) . userStatus .~ Doing goal

  let startFromNow :: Goal -> Goal
      startFromNow goal = goal {
        goalEnd = addUTCTime (goalLength goal) currentTime }

  let archiveGoal :: Goal -> GoalResult -> Telegram ()
      archiveGoal goal res = modifyMVar_ userDataVar $ \userData -> return $
        userData & ix (userId user) . userStatus .~ Resting
                 & ix (userId user) . archivedGoals %~ ((goal, res) :)

  lift $ case text of
    -- “?” means “query status”
    "?" -> do
      statusText <- liftIO $ showStatus status
      respond_ message statusText

    "done" -> case status of
      Resting ->
        respond_ message "but you weren't doing anything"
      Doing goal -> do
        let leftTime = diffUTCTime (goalEnd goal) (date message)
        if leftTime <= 0
          then archiveGoal goal Completed
          else do
            respond_ message $ format "okay, you finished {} early"
                                      [showDuration leftTime]
            archiveGoal goal (CompletedEarly leftTime)
      Judging goal ->
        archiveGoal goal Completed

    "fail" -> case status of
      Resting ->
        respond_ message "but you weren't doing anything"
      Doing goal -> do
        respond_ message "okay"
        archiveGoal goal Failed
      Judging goal ->
        archiveGoal goal Failed

    "again" -> case status of
      Doing _ ->
        respond_ message "you haven't finished the previous goal"
      Judging _ ->
        respond_ message "you still haven't said anything about \
                         \the goal you want to repeat; say “done, again” \
                         \or “fail, again”"
      Resting ->
        case thisUserData ^? archivedGoals . _head of
          Nothing ->
            respond_ message "can't do anything “again” since you \
                             \haven't ever scheduled any goals"
          Just (goal, _) -> do
            respond_ message $ quote (goalText goal) <>
                               blankline <>
                               "repeating this"
            setGoal (startFromNow goal)

    "done, again" -> case status of
      Resting ->
        respond_ message "but you weren't doing anything"
      Doing goal -> do
        let leftTime = diffUTCTime (goalEnd goal) (date message)
        if leftTime <= 0
          then archiveGoal goal Completed
          else do
            respond_ message $ format "okay, you finished {} early"
                                      [showDuration leftTime]
            archiveGoal goal (CompletedEarly leftTime)
        setGoal (startFromNow goal)
      Judging goal -> do
        archiveGoal goal Completed
        setGoal (startFromNow goal)

    "fail, again" -> case status of
      Resting ->
        respond_ message "but you weren't doing anything"
      Doing _ ->
        respond_ message "ignored (use reset instead)"
      Judging goal -> do
        archiveGoal goal Failed
        setGoal (startFromNow goal)

    "reset" -> case status of
      Resting ->
        respond_ message "but you weren't doing anything"
      Doing goal -> do
        respond_ message "okay"
        setGoal (startFromNow goal)
      Judging goal ->
        setGoal (startFromNow goal)

    "help" -> respond_ message helpText

    -- If the message a valid goal that can be parsed, either schedule it or
    -- say that the user already has an active/judged goal
    _ | Just (duration, goalText) <- parseGoal text ->
      case status of
        Doing   _ -> respond_ message "you already are doing something"
        Judging _ -> respond_ message "you still haven't said anything about \
                                      \the previous goal"
        Resting ->
          setGoal Goal {
            goalEnd         = addUTCTime duration currentTime,
            goalLength      = duration,
            originalMessage = message,
            goalText        = goalText }

    -- Otherwise, tell the user we couldn't parse the command
    _ -> respond_ message "couldn't parse what you said"

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
  "write something like “10m wash dishes” to set a goal, and in 10 minutes \
  \you'll be notified about it; then you can say:",
  "",
  "  • “done” if you have done it",
  "  • “fail” if you haven't",
  "  • “reset” if you forgot about the goal and want to restart it",
  "",
  "you can also say “done” and “reset” while the goal is in progress",
  "",
  "“fail, again” and “done, again” are useful when you want to repeat a goal \
  \that just finished; you can also say “again” to repeat the last goal no \
  \matter how long ago it was",
  "",
  "——————————",
  "",
  "to find out what you're supposed to be doing now, say “?”",
  "",
  "——————————",
  "",
  "to specify time you can also use stuff like “1h45m”, “3m30s”, and “1ч20м”" ]

-- Parsing

parseGoal :: Text -> Maybe (NominalDiffTime, Text)
parseGoal = parseMaybe goalP

goalP :: Parser (NominalDiffTime, Text)
goalP = do
  duration <- durationP
  skipSome spaceChar
  rest <- many anyChar
  return (duration, T.pack rest)

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

-- Utils

ignoreErrors :: Telegram a -> Telegram ()
ignoreErrors x = void x `catchError` (liftIO . print)

ignoreExceptions :: Telegram a -> Telegram ()
ignoreExceptions x = void x `catch` \(SomeException a) -> liftIO (print a)

showDuration :: NominalDiffTime -> Text
showDuration diff = do
  let (h, rest) = (ceiling diff :: Integer) `divMod` 3600
      (m, s)    = rest `divMod` 60
  if | h == 0 && m == 0 && s == 0 -> "nothing"
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
