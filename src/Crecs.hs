{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

module Crecs () where

import Crecs.Components

import Control.Monad.IO.Class
import Data.IORef
import Control.Monad.Trans.Reader

data Some c where
  Some :: c a => a -> Some c

data WorldContext = WorldContext {
  components :: IORef [IORef (Some Component)]
}

newtype World a = World { runWorld :: WorldContext -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO) via ReaderT WorldContext IO

-- Gets the internal context of a world
getWorldContext :: World WorldContext
getWorldContext = World (\ctx -> pure ctx)

newWorld :: IO WorldContext
newWorld = do
  comps <- newIORef []
  return WorldContext {
    components = comps
  }

addComponent :: (Component a) => a -> World ()
addComponent component = do
  context <- getWorldContext
  let comps = components context
  somed <- liftIO $ newIORef (Some component)
  liftIO $ modifyIORef comps (\x -> x ++ [somed])
  