module PrReminder.SlackId where

import Import

import Data.Aeson
import Database.Persist
import Database.Persist.Sql

newtype SlackId = SlackId Text
  deriving newtype
    (Show
    , Eq
    , Ord
    , PersistField
    , PersistFieldSql
    , FromJSON
    , ToJSON
    )
