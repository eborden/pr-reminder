module PrReminder.Github
where

import Import

import Control.Lens
import Data.Aeson
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Network.HTTP.Client (HttpException(..))
import Network.Wreq hiding (getWith)
import PrReminder.Http
import PrReminder.Ref
import PrReminder.Repo
import PrReminder.Url
import PrReminder.Username

-- GET /users/:username
data User = User
  { id :: Ref
  , login :: Username
  , html_url :: Url
  }
  deriving (Show, Generic, FromJSON)

getUser :: (MonadRepo m, MonadHttp m) => Text -> m (Either String User)
getUser username = getWithToken $ "/users/" <> T.unpack username

-- GET /teams/:id
data Team = Team
  { id :: Ref
  , name :: Text
  , url :: Url
  -- , member_url :: Text
  }
  deriving (Show, Generic, FromJSON)

newtype Label = Label
  { name :: Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)

-- GET /repos/:owner/:repo/issues/:number/events
data Event = Event
  { id :: Ref
  , event :: Text
  , actor :: User
  , created_at :: UTCTime
  , requested_reviewers :: Maybe [User]
  , labels :: Maybe [Label]
  }
  deriving (Show, Generic, FromJSON)

getEvents :: (MonadRepo m, MonadHttp m) => Natural -> m (Either String [Event])
getEvents number =
  getWithToken =<< prefixWithRepo ("/issues/" <> show number <> "/events")

-- GET /repos/:owner/:repo/pulls?status=open
data PR = PR
  { id :: Ref
  , number :: Natural
  , title :: Text
  , user :: User
  , html_url :: Url
  , labels :: [Label]
  }
  deriving (Show, Generic, FromJSON)

prefixWithRepo :: MonadRepo m => String -> m String
prefixWithRepo rest = do
  repo <- askRepo
  owner <- askOwner
  pure . T.unpack $ "/repos/" <> owner <> "/" <> repo <> T.pack rest

getPulls :: (MonadHttp m, MonadRepo m) => m (Either String [PR])
getPulls = getWithToken =<< prefixWithRepo "/pulls?status=open"

-- GET /repos/:owner/:repo/pulls/:number/requested_reviewers
data ReviewRequest = ReviewRequest
  { users :: [User]
  , teams :: [Team]
  }
  deriving (Show, Generic, FromJSON)

getReviewRequest
  :: (MonadHttp m, MonadRepo m) => Natural -> m (Either String ReviewRequest)
getReviewRequest num = getWithToken
  =<< prefixWithRepo ("/pulls/" <> show num <> "/requested_reviewers")

getReviewRequestUsers
  :: (MonadHttp m, MonadRepo m) => Natural -> m (Set Username)
getReviewRequestUsers num = do
  rev <- either (throwM . userError) pure =<< getReviewRequest num
  let usernames = Set.fromList $ view #login <$> users rev

  -- TODO handle teams
  {-
  (_eUsers :: [Either String [User]]) <-
    traverse getWithToken $ T.unpack . view #member_url <$> teams rev
  -}

  pure usernames

-- GET /repos/:owner/:repo/pulls/:number/reviews
newtype ReviewResponse = ReviewResponse
  { user :: User
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)

getReviewRespose
  :: (MonadHttp m, MonadRepo m) => Natural -> m (Either String [ReviewResponse])
getReviewRespose num =
  getWithToken =<< prefixWithRepo ("/pulls/" <> show num <> "/reviews")

getReviewResponseUsers
  :: (MonadHttp m, MonadRepo m) => Natural -> m (Set Username)
getReviewResponseUsers num = do
  Right resps <- getReviewRespose num
  pure . Set.fromList $ view (#user . #login) <$> resps

getWithToken
  :: (MonadRepo m, MonadHttp m, FromJSON a) => String -> m (Either String a)
getWithToken url = do
  token <- askToken
  let
    opts =
      defaults
        & auth
        ?~ basicAuth "eborden" (T.encodeUtf8 token)
        & checkResponse
        .~ Nothing
  go opts `catch` handler
 where
  go opts = do
    resp <- getWith opts $ mappend "https://api.github.com" url
    --traceM . show $ resp ^? responseLink "rel" "next" . linkURL
    pure $ eitherDecode $ view responseBody resp
  handler err@HttpExceptionRequest{} = pure . Left $ "http error: " <> show err
  handler err = pure . Left $ "unexpected error: " <> show err
