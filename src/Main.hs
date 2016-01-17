{-# LANGUAGE
RecordWildCards,
NoImplicitPrelude
  #-}


module Main where


-- General
import BasePrelude
-- Monad transformers
import Control.Monad.Except
-- Text
import qualified Data.Text.IO as T
-- Telegram
import Telegram


main :: IO ()
main = void $ do
  token <- T.readFile "telegram-token"
  runTelegram token $ echoMessages `catchError` (liftIO . print)

echoMessages :: Telegram ()
echoMessages = onUpdateLoop $ \Update{..} ->
  case text message of
    Nothing  -> return ()
    Just str -> void $ do
      liftIO $ T.putStrLn str
      respond message str
