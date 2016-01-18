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
      Just "?" -> void $ do
        reminders <- readMVar remindersVar
        respond message =<< liftIO (showStatus reminders)
      Just str -> void $ do
        case parseReminder str of
          Nothing -> respond message "couldn't parse what you said"
          Just (seconds, reminderText) -> do
            currentTime <- liftIO $ getCurrentTime
            let time = addUTCTime (fromIntegral seconds) currentTime
            schedule Reminder {
              time            = time,
              originalMessage = message,
              reminderText    = reminderText }
            respond message (format "scheduled at {}" [time])

-- Reminders

data Reminder = Reminder {
  time            :: UTCTime,
  originalMessage :: Message,
  reminderText    :: Text }

findDueReminders :: [Reminder] -> IO ([Reminder], [Reminder])
findDueReminders rs = do
  currentTime <- getCurrentTime
  return (partition ((< currentTime) . time) rs)

showStatus :: [Reminder] -> IO Text
showStatus rs = do
  currentTime <- getCurrentTime
  let showLeft r = showDuration (diffUTCTime (time r) currentTime) <> " left"
  return $ case rs of
    []  -> "you're not doing anything"
    [r] -> format "{} – {}" (reminderText r, showLeft r)
    _   -> T.unlines $ do
             (i, r) <- zip [1 :: Int ..] rs
             return (format "{}. {} – {}" (i, reminderText r, showLeft r))

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
