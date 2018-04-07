module PrReminder.Slack
  ( postEphemeral
  , Ephemeral(..)
  , SlackResponse(..)
  )
where

import Import

import Control.Lens
import Data.Aeson
import qualified Data.Text.Encoding as T
import Network.HTTP.Client (HttpException(..))
import Network.Wreq hiding (postWith)
import PrReminder.Http
import PrReminder.SlackId

data Ephemeral = Ephemeral
  { channel :: Text
  , text :: Text
  , user ::  SlackId
  , as_user ::  Bool
  }
  deriving (Generic, ToJSON)

data SlackResponse
  = Ok { message :: Text }
  | Err { message :: Text }
  deriving (Show, Generic)

instance FromJSON SlackResponse where
  parseJSON = withObject "SlackResponse" $ \o -> do
    ok <- o .: "ok"
    if ok then
      Ok <$> o .: "message_ts"
    else
      Err <$> o .: "error"

postEphemeral :: MonadHttp m => Text -> Ephemeral -> m (Either String SlackResponse)
postEphemeral = postWith' "/chat.postEphemeral"

postWith'
  :: (MonadHttp m, FromJSON a, ToJSON post)
  => String
  -> Text
  -> post
  -> m (Either String a)
postWith' url token postData = do
  let
    opts =
      defaults
        & set (header "Accept") ["application/json"]
        & set (header "Content-Type") ["application/json; charset=utf-8"]
        & set auth (Just $ oauth2Bearer $ T.encodeUtf8 token)
        & set checkResponse Nothing
  go opts `catch` handler
 where
  go opts = eitherDecode . view responseBody <$> postWith
    opts
    (mappend "https://slack.com/api" url)
    (toJSON postData)
  handler err@HttpExceptionRequest{} = pure . Left $ "http error: " <> show err
  handler err = pure . Left $ "unexpected error: " <> show err
