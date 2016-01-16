{-# LANGUAGE
OverloadedStrings,
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
import Data.Text (Text)
import qualified Data.Text.IO as T
-- ByteString
import qualified Data.ByteString.Char8 as B8

import Data.Aeson as Aeson
import Network.API.Builder hiding (routeURL)
import Network.HTTP.Client

-- yay for unsafety
token = unsafePerformIO $ T.readFile "telegram-token"

main :: IO ()
main = do
  let loop offset = do
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
        loop $ (succ <$> maxId) <|> offset
  either print void =<< runExceptT (loop Nothing)

data User = User {
  user_id    :: Integer,
  first_name :: Text,
  last_name  :: Maybe Text,
  username   :: Maybe Text }
  deriving (Show, Eq)

instance FromJSON User where
  parseJSON = withObject "user" $ \o -> do
    user_id    <- o .: "id"
    first_name <- o .: "first_name"
    last_name  <- optional (o .: "last_name")
    username   <- optional (o .: "username")
    return User{..}

data Message = Message {
  message_id :: Integer,
  from       :: Maybe User,
  chat       :: Chat,
  text       :: Maybe Text }
  deriving (Show, Eq)

instance FromJSON Message where
  parseJSON = withObject "message" $ \o -> do
    message_id <- o .: "message_id"
    from       <- o .: "from"
    chat       <- o .: "chat"
    text       <- optional (o .: "text")
    return Message{..}

data Chat = Chat {
  chat_id :: Integer }
  deriving (Show, Eq)

instance FromJSON Chat where
  parseJSON = withObject "chat" $ \o -> do
    chat_id <- o .: "id"
    return Chat{..}

data Update = Update {
  update_id :: Integer,
  message   :: Message } -- actually Maybe Message
  deriving (Show, Eq)

data Updates = Updates {
  updates :: [Update] }
  deriving (Show, Eq)

instance FromJSON Update where
  parseJSON = withObject "update" $ \o -> do
    update_id <- o .: "update_id"
    message   <- o .: "message"
    return Update{..}

instance FromJSON Updates where
  parseJSON x = do
    result <- parseJSON x <|>        -- if 0 results or more than 1 result
              (:[]) <$> parseJSON x  -- if exactly 1 result
    return (Updates result)

instance FromJSON a => Receivable (Wrapper a) where
  receive = useFromJSON

data Wrapper a = Wrapper {
  status :: Bool,
  result :: a }
  deriving (Show, Eq)

instance FromJSON a => FromJSON (Wrapper a) where
  parseJSON = withObject "wrapped" $ \o -> do
    status <- o .: "ok"
    guard (status == True)
    result <- o .: "result"
    return Wrapper{..}

{- This thing with customizeRequest is needed because Telegram API likes the former request but not the latter:

    https://api.telegram.org/bot<token>/getUpdates/?timeout=5
    https://api.telegram.org/bot<token>/getUpdates?timeout=5

-}
telegram :: Builder
telegram = (basicBuilder "Telegram API" "https://api.telegram.org") {
  _customizeRequest = \req -> req {
      path = if B8.last (path req) == '/' then B8.init (path req)
                                          else path req } }

getMeRoute :: Route
getMeRoute = Route {
  urlPieces  = [token, "getMe"],
  urlParams  = [],
  httpMethod = "GET" }

getUpdatesRoute :: Maybe Integer -> Route
getUpdatesRoute mbOffset = Route {
  urlPieces  = [token, "getUpdates"],
  urlParams  = [case mbOffset of Nothing -> []; Just x -> "offset" =. x],
  httpMethod = "GET" }

sendMessageRoute :: Integer -> Text -> Route
sendMessageRoute chat_id text = Route {
  urlPieces  = [token, "sendMessage"],
  urlParams  = ["chat_id" =. chat_id, "text" =. text],
  httpMethod = "GET" }

data Err = Err Text
  deriving Show

instance FromJSON Err where
  parseJSON = withObject "error" $ \o -> do
    status <- o .: "ok"
    descr  <- optional (o .: "description")
    guard (status == False)
    return (Err (fromMaybe "some error during the API call" descr))

instance ErrorReceivable Err where
  receiveError = useErrorFromJSON

getMe :: ExceptT (APIError Err) IO User
getMe = fmap result $ ExceptT $ execAPI telegram () $ runRoute getMeRoute

getUpdates :: Maybe Integer -> ExceptT (APIError Err) IO [Update]
getUpdates mbOffset = fmap (updates . result) $ ExceptT $ execAPI telegram () $ runRoute (getUpdatesRoute mbOffset)

sendMessage :: Integer -> Text -> ExceptT (APIError Err) IO Message
sendMessage chat_id text = fmap result $ ExceptT $ execAPI telegram () $ runRoute (sendMessageRoute chat_id text)
