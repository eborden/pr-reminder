module PrReminder.Url where

import Data.Aeson
import Data.Text (Text)

newtype Url = Url { unUrl :: Text }
  deriving newtype (FromJSON, Show)
