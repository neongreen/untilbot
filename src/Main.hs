{-# LANGUAGE
RecordWildCards,
NoImplicitPrelude
  #-}


module Main where


-- General
import BasePrelude
-- Monad transformers
import Control.Monad.IO.Class
-- Text
import qualified Data.Text.IO as T
-- Telegram
import Telegram


main :: IO ()
main = do
  token <- T.readFile "telegram-token"
  result <- runTelegram token (forever echoMessages)
  case result of
    Left err -> print err
    Right _  -> return ()

echoMessages :: Telegram ()
echoMessages = do
  messages <- map message <$> getUpdates
  for_ messages $ \Message{..} -> do
    case text of
      Nothing -> return ()
      Just s  -> void $ do
        liftIO $ T.putStrLn s
        sendMessage (chat_id chat) s
