module PrReminder.Http
  ( Wreq.Postable
  , MonadHttp(..)
  )
where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Types as Wreq

class (MonadCatch m, Monad m) => MonadHttp m where
  getWith :: Wreq.Options -> String -> m (Wreq.Response ByteString)
  postWith :: Wreq.Postable a => Wreq.Options -> String -> a -> m (Wreq.Response ByteString)

instance MonadHttp IO where
  getWith = Wreq.getWith
  postWith = Wreq.postWith

instance MonadHttp m => MonadHttp (ReaderT a m) where
  getWith o = lift . getWith o
  postWith o s = lift . postWith o s
