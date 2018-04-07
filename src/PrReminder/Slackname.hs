module PrReminder.Slackname where

import Import

import Data.String
import Data.Text (unpack)
import Database.Persist
import Database.Persist.Sql

newtype Slackname = Slackname Text
  deriving newtype (IsString, PersistField, PersistFieldSql)

instance Show Slackname where
  show (Slackname n) = unpack n
