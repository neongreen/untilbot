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
  originalMessage :: Message,
  goalText        :: Text }
  deriving (Show)

data UserData = UserData {
  _activeGoal     :: Maybe Goal,
  _completedGoals :: [Goal] }
  deriving (Show)

makeLenses ''UserData

instance Default UserData where
  def = UserData {
    _activeGoal     = Nothing,
    _completedGoals = [] }

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

-- Check every goal, send messages about ones that have ended, and leave
-- those that haven't ended yet in the map.
checkGoals :: MVar (Map UserId UserData) -> Telegram ()
checkGoals userDataVar = modifyMVar_ userDataVar $ \userData -> do
  currentTime <- liftIO getCurrentTime
  forM userData $ \ud ->
    case ud ^. activeGoal of
      Just goal | goalEnd goal < currentTime -> do
        respond (originalMessage goal) (quote (goalText goal))
        return $ ud & activeGoal .~ Nothing
                    & completedGoals %~ (goal:)
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
  -- nem. This makes 'schedule' and 'getActiveGoal' simpler, because they no
  -- longer have to care about missing entries.
  modifyMVar_ userDataVar $ \userData -> return $
    if M.member (user_id user) userData
      then userData
      else M.insert (user_id user) def userData

  let schedule :: Goal -> Telegram ()
      schedule goal = modifyMVar_ userDataVar $ \userData -> return $
        userData & ix (user_id user) . activeGoal .~ Just goal

  let getActiveGoal :: Telegram (Maybe Goal)
      getActiveGoal = do
        userData <- readMVar userDataVar
        return $ userData ^?! ix (user_id user) . activeGoal

  lift $ case text of
    -- “?” means “query status”; show active goal to the user
    "?" -> void $ do
      mbActiveGoal <- getActiveGoal
      status <- liftIO $ showStatus mbActiveGoal
      respond message status

    -- If the message a valid goal that can be parsed, either schedule it or
    -- say that the user already has an active goal.
    _ | Just (seconds, goalText) <- parseGoal text -> do
      mbActiveGoal <- getActiveGoal
      case mbActiveGoal of
        Just _  -> void $ respond message "you already are doing something"
        Nothing -> do
          currentTime <- liftIO $ getCurrentTime
          let endTime = addUTCTime (fromIntegral seconds) currentTime
          schedule Goal {
            goalEnd         = endTime,
            originalMessage = message,
            goalText        = goalText }

    -- Otherwise, tell the user we couldn't parse the command
    _ -> void $ respond message "couldn't parse what you said"

showStatus :: Maybe Goal -> IO Text
showStatus Nothing  = return "you're not doing anything"
showStatus (Just x) = do
  currentTime <- getCurrentTime
  let leftTime = showDuration (diffUTCTime (goalEnd x) currentTime)
  return $ format "{}\n{} left" (quote (goalText x), leftTime)

-- Parsing

parseGoal :: Text -> Maybe (Integer, Text)
parseGoal = parseMaybe goalP

goalP :: Parser (Integer, Text)
goalP = do
  duration <- durationP
  skipSome spaceChar
  rest <- many anyChar
  return (duration, T.pack rest)

durationP :: Parser Integer
durationP = do
  items <- some $ do
    n <- integer
    let strings = choice . map string . words
    choice [
      strings "h hr  ч час" *> pure (n*3600),
      strings "m min м мин" *> pure (n*60),
      strings "s sec с сек" *> pure n ]
  return (sum items)

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

liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return
