{-# LANGUAGE
RecordWildCards,
OverloadedStrings,
MultiWayIf,
NoImplicitPrelude
  #-}


module Main where


-- General
import BasePrelude hiding (
  threadDelay,
  catch,
  readMVar, takeMVar, newMVar, putMVar, modifyMVar_ )
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


main :: IO ()
main = void $ do
  botToken <- T.readFile "telegram-token"
  runTelegram botToken bot

bot :: Telegram ()
bot = do
  -- Create a variable holding all goals.
  goalsVar <- newMVar mempty
  -- Run the loop that checks all goals every second, sends messages
  -- about ones that have ended, and leaves those that haven't ended yet.
  fork $ forever $ ignoreErrors $ do
    modifyMVar_ goalsVar $ \goals -> do
      (finished, inProgress) <- liftIO $ findFinishedGoals goals
      for_ finished $ \Goal{..} ->
        respond originalMessage (quote goalText)
      return inProgress
    threadDelay 1000000
  -- Run the loop that accepts incoming messages.
  onUpdateLoop $ \Update{..} ->
    ignoreErrors $ ignoreExceptions $ processMessage goalsVar message

processMessage :: MVar (Map UserId Goal) -> Message -> Telegram ()
processMessage goalsVar message = void $ runMaybeT $ do
  -- Both user and message text have to be present (otherwise we don't do
  -- anything).
  -- 
  -- from message :: Maybe User
  -- text message :: Maybe Text
  -- 
  -- Read about MaybeT if you don't understand how this works.
  user <- liftMaybe (from message)
  text <- liftMaybe (text message)

  let schedule :: Goal -> Telegram ()
      schedule goal = modifyMVar_ goalsVar $ \m ->
        return (M.insert (user_id user) goal m)

  let getActiveGoal :: Telegram (Maybe Goal)
      getActiveGoal = M.lookup (user_id user) <$> readMVar goalsVar

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

-- Goals

data Goal = Goal {
  goalEnd         :: UTCTime,
  originalMessage :: Message,
  goalText        :: Text }

findFinishedGoals :: Map UserId Goal
                  -> IO (Map UserId Goal, Map UserId Goal)
findFinishedGoals m = do
  currentTime <- getCurrentTime
  return (M.partition ((< currentTime) . goalEnd) m)

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
