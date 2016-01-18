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
  -- Create a variable holding all goals.
  goalsVar <- newMVar []
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

processMessage :: MVar [Goal] -> Message -> Telegram ()
processMessage goalsVar message = do
  let schedule :: Goal -> Telegram ()
      schedule x = modifyMVar_ goalsVar (\xs -> return (x:xs))

  case text message of
    -- A sound or a sticker or whatever instead of text; ignoring
    Nothing -> return ()

    -- “?” means “query status”; show active goal to the user
    Just "?" -> void $ do
      goals <- readMVar goalsVar
      let thisChat = chat_id (chat message)
          goalChat = chat_id . chat . originalMessage
      status <- liftIO $ showStatus (filter ((== thisChat) . goalChat) goals)
      respond message status

    -- If it's a valid goal that can be parsed, schedule it
    Just str | Just (seconds, goalText) <- parseGoal str -> do
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

findFinishedGoals :: [Goal] -> IO ([Goal], [Goal])
findFinishedGoals xs = do
  currentTime <- getCurrentTime
  return (partition ((< currentTime) . goalEnd) xs)

showStatus :: [Goal] -> IO Text
showStatus xs = do
  currentTime <- getCurrentTime
  let showLeft x = showDuration (diffUTCTime (goalEnd x) currentTime)
  return $ case xs of
    []  -> "you're not doing anything"
    [x] -> format "{}\n{} left" (quote (goalText x), showLeft x)
    _   -> "somehow you managed to be doing more than one goal at once, \
           \it's a bug"

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
