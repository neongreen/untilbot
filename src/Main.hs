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
  token <- T.readFile "telegram-token"
  runTelegram token bot

ignoreErrors :: Telegram a -> Telegram ()
ignoreErrors x = void x `catchError` (liftIO . print)

ignoreExceptions :: Telegram a -> Telegram ()
ignoreExceptions x = void x `catch` \(SomeException a) -> liftIO (print a)

data Reminder = Reminder {
  time   :: UTCTime,
  chatId :: Integer,
  msg    :: Text }

bot :: Telegram ()
bot = do
  remindersVar <- newMVar []
  fork $ forever $ do
    modifyMVar_ remindersVar $ \reminders -> do
      currentTime <- liftIO $ getCurrentTime
      let (fired, later) = partition ((< currentTime) . time) reminders
      for_ fired $ \Reminder{..} -> ignoreErrors $
        sendMessage chatId msg
      return later
    threadDelay 1000000
  let schedule :: Reminder -> Telegram ()
      schedule r = modifyMVar_ remindersVar (return . (r:))
  onUpdateLoop $ \Update{..} -> ignoreErrors $ ignoreExceptions $ do
    case text message of
      Nothing ->
        return ()
      Just str -> void $ do
        let (d, rest) = T.break (== ' ') str
        currentTime <- liftIO $ getCurrentTime
        let seconds = read (T.unpack d) :: Integer
            reminderTime = addUTCTime (fromIntegral seconds) currentTime
        schedule Reminder {
          time   = reminderTime,
          chatId = chat_id (chat message),
          msg    = T.tail rest }
        respond message ("scheduled at " <> T.pack (show reminderTime))
