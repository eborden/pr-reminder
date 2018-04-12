module PrReminder
  ( main
  )
where

import Import

import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Conduit
import qualified Data.Conduit.List as C
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sqlite
import PrReminder.DB
import PrReminder.Github
import PrReminder.Http
import PrReminder.Repo
import PrReminder.Slack
import PrReminder.SlackId
import PrReminder.Slackname
import PrReminder.Url
import PrReminder.Username

main :: IO ()
main = do
  clients <- runSqlite dbName $ do
    runMigration migrateAll
    fmap entityVal <$> selectList [] []
  withApps $ \app ->
    runStderrLoggingT . (`runReaderT` app) . run $ sendDigest clients

withApps :: (App -> IO b) -> IO ()
withApps f = do
  apps <- runSqlite @IO @SqlBackend dbName $ fmap entityVal <$> selectList [] []
  for_ apps $ \repo -> f App
    { repo = repoName repo
    , owner = repoOwner repo
    , token = repoToken repo
    , slackToken = repoSlackToken repo
    }

dbName :: Text
dbName = "pr-reminder.sqlite"

data App = App
  { repo :: Text
  , owner :: Text
  , token :: Text
  , slackToken :: Text
  }
  deriving (Generic)

newtype Run a = Run { run :: ReaderT App (LoggingT IO) a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadLogger
    , MonadReader App
    , MonadHttp
    , MonadThrow
    , MonadCatch
    , MonadIO
    )

instance MonadRepo Run where
  askRepo = asks $ view #repo
  askOwner = asks $ view #owner
  askToken = asks $ view #token
  askSlackToken = asks $ view #slackToken

sendDigest :: (MonadLogger m, MonadRepo m, MonadHttp m) => [Client] -> m ()
sendDigest clients = do
  let
    slackMap = assocUsernameToSlack clients
    send (pull, usernames) = do
      let msg = reminderMsg slackMap pull usernames
      sendSlack slackMap usernames msg
      logInfoN msg
  runConduit
    $ fetchPRs
    .| C.mapM getReviews
    .| C.filter (not . null . snd)
    .| C.mapM send
    .| C.sinkNull

sendSlack
  :: (MonadRepo m, MonadHttp m)
  => Map Username (Slackname, SlackId)
  -> Set Username
  -> Text
  -> m ()
sendSlack slackMap usernames msg = do
  slackToken <- askSlackToken
  for_ usernames $ \name -> case Map.lookup name slackMap of
    Nothing -> pure ()
    Just (_, slackId) -> when (name == "eborden") $ void $ postEphemeral
      slackToken
      Ephemeral {channel = "#test", text = msg, user = slackId, as_user = True}

reminderMsg :: Map Username (Slackname, SlackId) -> PR -> Set Username -> Text
reminderMsg slackMap pull usernames = T.strip $ T.unlines
  [ title pull
  , "<" <> unUrl (view #html_url pull) <> ">"
  , "Pending Review: " <> T.intercalate ", " usernamesWithSlack
  ]
 where
  usernamesWithSlack = toList usernames <&> \name ->
    maybe (tshow name) (("@" <>) . tshow . fst) $ Map.lookup name slackMap

assocUsernameToSlack :: [Client] -> Map Username (Slackname, SlackId)
assocUsernameToSlack clients = Map.fromList
  [ (u, (slackName, slackId))
  | c <- clients
  , let u = clientUsername c
  , let Just slackName = clientSlackname c
  , let Just slackId = clientSlackId c
  ]

fetchPRs :: (MonadRepo m, MonadHttp m) => ConduitT i PR m ()
fetchPRs = do
  pulls <- lift $ either (throwM . userError) pure =<< getPulls
  C.sourceList pulls .| C.filter (not . hasLabels ignoreLabels)

getReviews :: (MonadRepo m, MonadHttp m) => PR -> m (PR, Set Username)
getReviews pull = do
  reqUsers <- getReviewRequestUsers $ view #number pull
  respUsers <- getReviewResponseUsers $ view #number pull
  let pendingReview = Set.difference reqUsers respUsers
  pure (pull, Set.map snd pendingReview)

hasLabels :: [Text] -> PR -> Bool
hasLabels labels pr = any (`elem` labels) $ view #name <$> view #labels pr

ignoreLabels :: [Text]
ignoreLabels = ["WIP", "Blocked"]
