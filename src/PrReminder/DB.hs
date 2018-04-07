{-# LANGUAGE NoDeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module PrReminder.DB where

import Import

import Database.Persist.TH
import PrReminder.SlackId
import PrReminder.Slackname
import PrReminder.Username

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Client
  username Username
  slackname Slackname Maybe
  slackId SlackId Maybe
  deriving Generic

Repo
  owner Text
  name Text
  token Text
  slackToken Text
  deriving Generic Show

ClientRepo
  client ClientId
  repo RepoId
  deriving Generic
|]
