module PrReminder.Http where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import qualified Network.Wreq as Wreq

class (MonadCatch m, Monad m) => MonadHttp m where
  getWith :: Wreq.Options -> String -> m (Wreq.Response ByteString)

instance MonadHttp IO where
  getWith = Wreq.getWith

instance MonadHttp m => MonadHttp (ReaderT a m) where
  getWith o s = lift $ getWith o s
