{-# LANGUAGE
OverloadedStrings,
RecordWildCards,
NoImplicitPrelude
  #-}


module Telegram where


-- General
import BasePrelude
-- Monads
import Control.Monad.State
-- Text
import Data.Text (Text)
-- ByteString
import qualified Data.ByteString.Char8 as B8
-- JSON
import Data.Aeson as Aeson
-- interaction with Telegram API
import Network.API.Builder
import Network.HTTP.Client


type Token = Text

type Telegram a = API TelegramState Err a

-- TODO: save state somewhere so that it'd be preserved on exit
runTelegram :: Token -> Telegram a -> IO (Either (APIError Err) a)
runTelegram token = execAPI (telegram token) TelegramState {
  nextOffset = Nothing }

data TelegramState = TelegramState {
  nextOffset :: Maybe Integer }

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
  chat_id :: Integer }   -- TODO: or can be String
  deriving (Show, Eq)

instance FromJSON Chat where
  parseJSON = withObject "chat" $ \o -> do
    chat_id <- o .: "id"
    return Chat{..}

data Update = Update {
  update_id :: Integer,
  -- TODO: actually Maybe Message
  message   :: Message }
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
telegram :: Token -> Builder
telegram token = (basicBuilder "Telegram API" url) {
  _customizeRequest = \req -> req {
      path = if B8.last (path req) == '/' then B8.init (path req)
                                          else path req } }
  where
    url = "https://api.telegram.org/bot" <> token

getMeRoute :: Route
getMeRoute = Route {
  urlPieces  = ["getMe"],
  urlParams  = [],
  httpMethod = "GET" }

getUpdatesRoute :: Maybe Integer -> Route
getUpdatesRoute mbOffset = Route {
  urlPieces  = ["getUpdates"],
  urlParams  = [case mbOffset of Nothing -> []; Just x -> "offset" =. x],
  httpMethod = "GET" }

sendMessageRoute :: Integer -> Text -> Route
sendMessageRoute chat_id text = Route {
  urlPieces  = ["sendMessage"],
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

getMe :: Telegram User
getMe = result <$> runRoute getMeRoute

getUpdates_ :: Maybe Integer -> Telegram [Update]
getUpdates_ mbOffset = updates . result <$> runRoute (getUpdatesRoute mbOffset)

getUpdates :: Telegram [Update]
getUpdates = do
  mbOffset <- liftState $ gets nextOffset
  updates <- getUpdates_ mbOffset
  let maxId = case updates of
        [] -> Nothing
        _  -> Just $ maximum (map update_id updates)
  liftState $ modify $ \s -> s {nextOffset = max (succ <$> maxId) mbOffset}
  return updates

-- | Note that this will ignore all non-message updates (such as inline queries).
getMessages :: Telegram [Message]
getMessages = map message <$> getUpdates

sendMessage :: Integer -> Text -> Telegram Message
sendMessage chat_id text = result <$> runRoute (sendMessageRoute chat_id text)

respond :: Message -> Text -> Telegram Message
respond msg text = sendMessage (chat_id (chat msg)) text

onUpdateLoop :: (Update -> Telegram a) -> Telegram ()
onUpdateLoop handler = forever $ do
  updates <- getUpdates
  mapM_ handler updates
