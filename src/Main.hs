{-# LANGUAGE
RecordWildCards,
OverloadedStrings,
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
-- Text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
  -- Create a variable holding all reminders.
  remindersVar <- newMVar []
  let schedule :: Reminder -> Telegram ()
      schedule r = modifyMVar_ remindersVar (return . (r:))
  -- Run the loop that checks all reminders every second, sends messages
  -- about ones that have fired, and leaves those that haven't fired yet.
  fork $ forever $ ignoreErrors $ do
    modifyMVar_ remindersVar $ \reminders -> do
      (fired, later) <- liftIO $ findDueReminders reminders
      for_ fired $ \Reminder{..} -> reply originalMessage "reminding"
      return later
    threadDelay 1000000
  -- Run the loop that accepts incoming messages.
  onUpdateLoop $ \Update{..} -> ignoreErrors $ ignoreExceptions $ do
    case text message of
      Nothing ->
        return ()
      Just str -> void $ do
        case parseReminder str of
          Nothing -> respond message "couldn't parse what you said"
          Just (seconds, _) -> do
            currentTime <- liftIO $ getCurrentTime
            let time = addUTCTime (fromIntegral seconds) currentTime
            schedule Reminder {
              time            = time,
              originalMessage = message }
            respond message ("scheduled at " <> T.pack (show time))

-- Reminders

data Reminder = Reminder {
  time            :: UTCTime,
  originalMessage :: Message }

findDueReminders :: [Reminder] -> IO ([Reminder], [Reminder])
findDueReminders rs = do
  currentTime <- getCurrentTime
  return (partition ((< currentTime) . time) rs)

-- Parsing

parseReminder :: Text -> Maybe (Integer, Text)
parseReminder = parseMaybe reminderP

reminderP :: Parser (Integer, Text)
reminderP = do
  duration <- durationP
  skipSome spaceChar
  rest <- many anyChar
  return (duration, T.pack rest)

durationP :: Parser Integer
durationP = do
  items <- some $ do
    n <- integer
    choice [
      string "h" *> pure (n*3600),
      string "m" *> pure (n*60),
      string "s" *> pure n ]
  return (sum items)

-- Utils

ignoreErrors :: Telegram a -> Telegram ()
ignoreErrors x = void x `catchError` (liftIO . print)

ignoreExceptions :: Telegram a -> Telegram ()
ignoreExceptions x = void x `catch` \(SomeException a) -> liftIO (print a)
