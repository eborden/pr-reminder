module PrReminder.Username where

import Import

import Data.Aeson
import Data.String
import Database.Persist
import Database.Persist.Sql

newtype Username = Username Text
  deriving newtype (Show, IsString, Eq, Ord, PersistField, PersistFieldSql, FromJSON)
