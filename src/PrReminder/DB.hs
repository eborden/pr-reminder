{-# LANGUAGE NoDeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module PrReminder.DB where

import Import

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Client
  username Text
  slackname Text Maybe
  deriving Generic

Repo
  owner Text
  name Text
  token Text
  deriving Generic Show

ClientRepo
  client ClientId
  repo RepoId
  deriving Generic
|]
