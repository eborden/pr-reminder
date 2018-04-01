module PrReminder.Link
  ( module PrReminder.Link
  , parse
  , ParseError
  )where

import Data.Functor (void)
import Data.List (find)
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import PrReminder.Url
import Text.Megaparsec
import Text.Megaparsec.Char

-- Link: <https://api.github.com/resource?page=2>; rel="next",
--       <https://api.github.com/resource?page=5>; rel="last"
data Link = Link
  { next :: Maybe Url
  , last :: Maybe Url
  }
  deriving (Show)

data LinkRaw = LinkRaw
  { url :: Url
  , rel :: Text
  }

test :: Either (ParseError Char Void) Link
test = parse parseLink "test"
  $ "<https://api.github.com/resource?page=2>; rel=\"next\","
  <> " <https://api.github.com/resource?page=5>; rel=\"last\""

parseLink :: Parsec Void Text Link
parseLink = do
  links <- parseLinksRaw
  pure Link
    { next = getUrl "next" links
    , last = getUrl "last" links
    }
 where
  getUrl name = fmap url . find ((== name) . rel)

parseLinkRaw :: Parsec Void Text LinkRaw
parseLinkRaw = do
  void $ char '<'
  url <- Url . T.pack <$> manyTill anyChar (char '>')
  void $ string "; rel=\""
  rel <- T.pack <$> manyTill anyChar (char '\"')
  pure LinkRaw{url, rel}

parseLinksRaw :: Parsec Void Text [LinkRaw]
parseLinksRaw =
  many spaceChar
    *> parseLinkRaw `sepBy` (char ',' *> many spaceChar)
    <* many spaceChar
