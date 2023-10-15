{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crecs () where

import Crecs.Components

import Control.Monad.IO.Class
import Data.IORef
import Control.Monad.Trans.Reader
import Data.Typeable
import Data.Maybe
import GHC.Utils.Monad

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

addComponent :: (Typeable a, Component a) => a -> World ()
addComponent component = do
  context <- getWorldContext
  let comps = components context
  somed <- liftIO $ newIORef (Some component)
  liftIO $ modifyIORef comps (\x -> x ++ [somed])

getComponents :: forall a. (Typeable a, Component a) => World [IORef a]
getComponents = do
  context <- getWorldContext
  let comps = components context
  readComps <- liftIO $ readIORef comps
  liftIO $ mapMaybeM (\x -> modifyIORef (\(Some y) -> cast y) x) readComps
