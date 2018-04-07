module PrReminder.Username where

import Import

import Data.Aeson
import Data.String
import Data.Text (unpack)
import Database.Persist
import Database.Persist.Sql

newtype Username = Username Text
  deriving newtype (IsString, Eq, Ord, PersistField, PersistFieldSql, FromJSON)

instance Show Username where
  show (Username n) = unpack n
