module PrReminder
  ( main
  )
where

import Import

import Control.Lens
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Persist
import Database.Persist.Sqlite
import PrReminder.DB
import PrReminder.Github
import PrReminder.Http
import PrReminder.Repo
import PrReminder.Slack
import PrReminder.Url

main :: IO ()
main = do
  clients <- runSqlite dbName $ do
    runMigration migrateAll
    fmap entityVal <$> selectList [] []
  withApps $ \app ->
    (`runReaderT` app) . run $ printDigest clients

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

newtype Run a = Run { run :: ReaderT App IO a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
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

printDigest :: (MonadIO m, MonadRepo m, MonadHttp m) => [Client] -> m ()
printDigest clients = do
  let
    slackMap = Map.fromList
      [ (u, (slackName, slackId))
      | c <- clients
      , let u = clientUsername c
      , let Just slackName = clientSlackname c
      , let Just slackId = clientSlackId c
      ]
  slackToken <- askSlackToken
  (pullMap, usernameMap) <- digest
  liftIO $ do
    putStrLn $ show (length pullMap) <> " open pull requests"
    for_ (Map.toList usernameMap) $ \(pullnum, usernames) -> do
      let
        Just pull = Map.lookup pullnum pullMap
        usernamesWithSlack =
          usernames <&> \name -> maybe name (("@" <>) . fst) $ Map.lookup name slackMap
        msg = T.unlines
          [ title pull
          , "<" <> unUrl (view #html_url pull) <> ">"
          , "Pending Review: " <> T.intercalate ", " usernamesWithSlack
          ]

      T.putStrLn msg

      for_ usernames $ \name ->
        case Map.lookup name slackMap of
          Nothing -> pure ()
          Just (_, slackId) -> when (name == "eborden") $
            print =<< postEphemeral slackToken Ephemeral
              { channel = "#test"
              , text = msg
              , user = slackId
              , as_user = True
              }

digest :: (MonadRepo m, MonadHttp m) => m (Map Natural PR, Map Natural [Text])
digest = do
  pulls <- either (throwM . userError) pure =<< getPulls
  reqUsers <- fmap mconcat . for pulls $ getReviewRequestUsers . view #number
  respUsers <- fmap mconcat . for pulls $ getReviewResponseUsers . view #number
  let
    pullMap = Map.fromList $ (view #number &&& Prelude.id) <$> pulls
    pendingReview = Set.difference reqUsers respUsers
    usernameMap =
      Map.fromListWith mappend $ second (: []) <$> Set.toList pendingReview
  pure (pullMap, usernameMap)
