module PrReminder.Ref where

import Import

import Data.Aeson
import Database.Persist.Sql

newtype Ref = Ref Int
  deriving (Eq, Ord, Generic)
  deriving newtype (Show, FromJSON, PersistField, PersistFieldSql)
