{-# LANGUAGE
RecordWildCards,
NoImplicitPrelude
  #-}


module Main where


-- General
import BasePrelude
-- Monad transformers
import Control.Monad.Reader
import Control.Monad.Except
-- Text
import qualified Data.Text.IO as T
-- Telegram
import Telegram


main :: IO ()
main = do
  token <- T.readFile "telegram-token"
  let go offset = do
        msgs <- getUpdates offset
        for_ msgs $ \Update{..} ->
          case text message of
            Nothing -> return ()
            Just s  -> do
              liftIO $ T.putStrLn s
              sendMessage (chat_id (chat message)) s
              return ()
        let maxId = case msgs of
              [] -> Nothing
              _  -> Just $ maximum (map update_id msgs)
        go $ (succ <$> maxId) <|> offset
  either print void =<< runExceptT (runReaderT (go Nothing) token)
