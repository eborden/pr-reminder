module PrReminder.SlackId where

import Import

import Data.Aeson
import Data.Text (unpack)
import Database.Persist
import Database.Persist.Sql

newtype SlackId = SlackId Text
  deriving newtype
    ( Eq
    , Ord
    , PersistField
    , PersistFieldSql
    , FromJSON
    , ToJSON
    )

instance Show SlackId where
  show (SlackId n) = unpack n
