{-# LANGUAGE
OverloadedStrings,
RecordWildCards,
TemplateHaskell,
NoImplicitPrelude
  #-}


module Telegram
(
  module Data.Default.Class,

  -- * Running API
  Token,
  Telegram,
  runTelegram,

  -- * Types
  UserId, ChatId, MessageId, UpdateId,
  User(..),
  Chat(..),
  Update(..),
  Message(..),

  -- * Methods and operations
  getMe,
  getUpdates,
  getMessages,
  SendMessageParams(..),
  sendMessage, sendMessage_, sendMessage', sendMessage'_,
  respond, respond_, respond', respond'_,
  reply,
)
where


-- General
import BasePrelude
-- Monads
import Control.Monad.State
-- Text
import Data.Text (Text)
import qualified Data.Text as T
-- ByteString
import qualified Data.ByteString.Char8 as B8
-- JSON
import Data.Aeson as Aeson
-- Default
import Data.Default.Class
-- Time
import Data.Time
import Data.Time.Clock.POSIX
-- safecopy
import Data.SafeCopy
-- interaction with Telegram API
import Network.API.Builder
import Network.HTTP.Client


type Token = Text

type UserId = Integer

data ChatId = ChatId Integer | ChannelId Text
  deriving (Eq, Show)

instance FromJSON ChatId where
  parseJSON x = asum [
    ChatId    <$> parseJSON x,
    ChannelId <$> parseJSON x ]

deriveSafeCopy 1 'base ''ChatId

type MessageId = Integer

type UpdateId = Integer

type Telegram a = API TelegramState Err a

-- TODO: save state somewhere so that it'd be preserved on exit
runTelegram :: Token -> Telegram a -> IO (Either (APIError Err) a)
runTelegram token = execAPI (telegram token) TelegramState {
  nextOffset = Nothing }

data TelegramState = TelegramState {
  nextOffset :: Maybe UpdateId }

data User = User {
  userId    :: UserId,
  firstName :: Text,
  lastName  :: Maybe Text,
  username  :: Maybe Text }
  deriving (Show, Eq)

instance FromJSON User where
  parseJSON = withObject "user" $ \o -> do
    userId    <- o .: "id"
    firstName <- o .: "first_name"
    lastName  <- optional (o .: "last_name")
    username  <- optional (o .: "username")
    return User{..}

data Message = Message {
  messageId :: MessageId,
  from      :: Maybe User,
  date      :: UTCTime,
  chat      :: Chat,
  text      :: Maybe Text }
  deriving (Show, Eq)

instance FromJSON Message where
  parseJSON = withObject "message" $ \o -> do
    messageId <- o .: "message_id"
    from      <- o .: "from"
    date      <- posixSecondsToUTCTime . fromInteger <$> o .: "date"
    chat      <- o .: "chat"
    text      <- optional (o .: "text")
    return Message{..}

data Chat = Chat {
  chatId :: ChatId }
  deriving (Show, Eq)

instance FromJSON Chat where
  parseJSON = withObject "chat" $ \o -> do
    chatId <- o .: "id"
    return Chat{..}

data Update = Update {
  updateId :: UpdateId,
  message  :: Message }  -- TODO: actually Maybe Message
  deriving (Show, Eq)

data Updates = Updates {
  updates :: [Update] }
  deriving (Show, Eq)

instance FromJSON Update where
  parseJSON = withObject "update" $ \o -> do
    updateId <- o .: "update_id"
    message  <- o .: "message"
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

getUpdatesRoute :: Maybe UpdateId -> Route
getUpdatesRoute mbOffset = Route {
  urlPieces  = ["getUpdates"],
  urlParams  = ["offset" =. mbOffset],
  httpMethod = "GET" }

sendMessageRoute :: ChatId -> Text -> SendMessageParams -> Route
sendMessageRoute chatId text SendMessageParams{..} = Route {
  urlPieces  = ["sendMessage"],
  urlParams  = [
      "chat_id" =. case chatId of
                     ChatId    x -> T.pack (show x)
                     ChannelId x -> x,
      "text" =. text,
      "parse_mode" =. case parseMode of
                        Normal   -> Nothing
                        Markdown -> Just ("Markdown" :: Text),
      "disable_web_page_preview" =. disableWebPagePreview,
      "reply_to_message_id"      =. replyTo ],
  httpMethod = "POST" }

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

getUpdates_ :: Maybe UpdateId -> Telegram [Update]
getUpdates_ mbOffset = updates . result <$> runRoute (getUpdatesRoute mbOffset)

getUpdates :: Telegram [Update]
getUpdates = do
  mbOffset <- liftState $ gets nextOffset
  updates <- getUpdates_ mbOffset
  let maxId = case updates of
        [] -> Nothing
        _  -> Just $ maximum (map updateId updates)
  liftState $ modify $ \s -> s {nextOffset = max (succ <$> maxId) mbOffset}
  return updates

-- | Note that this will ignore all non-message updates (such as inline queries).
getMessages :: Telegram [Message]
getMessages = map message <$> getUpdates

data ParseMode = Normal | Markdown
  deriving (Eq, Show)

data SendMessageParams = SendMessageParams {
  parseMode             :: ParseMode,
  disableWebPagePreview :: Bool,
  replyTo               :: Maybe MessageId }

instance Default SendMessageParams where
  def = SendMessageParams {
    parseMode             = Normal,
    disableWebPagePreview = False,
    replyTo               = Nothing }

sendMessage :: ChatId -> Text -> Telegram Message
sendMessage chatId text = sendMessage' chatId text def

sendMessage_ :: ChatId -> Text -> Telegram ()
sendMessage_ chatId text = void $ sendMessage chatId text

sendMessage' :: ChatId -> Text -> SendMessageParams -> Telegram Message
sendMessage' chatId text params =
  result <$> runRoute (sendMessageRoute chatId text params)

sendMessage'_ :: ChatId -> Text -> SendMessageParams -> Telegram ()
sendMessage'_ chatId text params = void $ sendMessage' chatId text params

respond :: Message -> Text -> Telegram Message
respond msg text = respond' msg text def

respond_ :: Message -> Text -> Telegram ()
respond_ msg text = void $ respond msg text

respond' :: Message -> Text -> SendMessageParams -> Telegram Message
respond' msg text params = sendMessage' (chatId (chat msg)) text params

respond'_ :: Message -> Text -> SendMessageParams -> Telegram ()
respond'_ msg text params = void $ respond' msg text params

reply :: Message -> Text -> Telegram Message
reply msg text = respond' msg text def{
  replyTo = Just (messageId msg) }
