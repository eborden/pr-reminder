module PrReminder.Repo where

import Import

class Monad m => MonadRepo m where
  askRepo :: m Text
  askOwner :: m Text
  askToken :: m Text
