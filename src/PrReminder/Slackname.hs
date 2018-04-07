module PrReminder.Slackname where

import Import

import Data.String
import Database.Persist
import Database.Persist.Sql

newtype Slackname = Slackname Text
  deriving newtype (Show, IsString, PersistField, PersistFieldSql)
