module PrReminder.Slack
  ( postEphemeral
  , Ephemeral(..)
  )
where

import Import

import Control.Lens
import Data.Aeson
import Network.HTTP.Client (HttpException(..))
import Network.Wreq hiding (postWith)
import PrReminder.Http

data Ephemeral = Ephemeral
  { token :: Text
  , channel :: Text
  , text :: Text
  , user ::  Text
  }
  deriving (Generic, ToJSON)

postEphemeral :: MonadHttp m => Ephemeral -> m (Either String ())
postEphemeral = postWith' "/chat.postEphemeral"

postWith'
  :: (MonadHttp m, FromJSON a, ToJSON post)
  => String
  -> post
  -> m (Either String a)
postWith' url postData = do
  let
    opts =
      defaults
        & set (header "Accept") ["application/json"]
        & set checkResponse Nothing
  go opts `catch` handler
 where
  go opts = eitherDecode . view responseBody <$> postWith
    opts
    (mappend "https://api.slack.com/api" url)
    (toJSON postData)
  handler err@HttpExceptionRequest{} = pure . Left $ "http error: " <> show err
  handler err = pure . Left $ "unexpected error: " <> show err
