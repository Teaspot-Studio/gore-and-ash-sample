{-|
Module      : Game.GoreAndAsh.ModuleSample.Module
Description : Internal implementation of public API of sample game module
Copyright   : (c) Anton Gushcha, 2016-2017
                  Anatoly Nardid, 2016-2017
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module defines implementation of sample game module. You are
interested only in a 'ModuleSampleT' and 'ModuleSampleOptions' types as 'ModuleSampleT'
should be placed in your monad stack to enable 'MonadModuleSample' API in your
application.

@
type AppStack t = ModuleSampleT t (LoggingT t (TimerT t (GameMonad t)))

newtype AppMonad t a = AppMonad { runAppMonad :: AppStack t a}
  deriving (Functor, Applicative, Monad, MonadFix)
@

And you will need some boilerplate code for instance deriving, see
`examples/Example01.hs` for full example.

-}
module Game.GoreAndAsh.ModuleSample.Module(
    ModuleSampleOptions(..)
  , ModuleSampleT(..)
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Proxy

import Game.GoreAndAsh
import Game.GoreAndAsh.ModuleSample.API

-- | Options that are passed to 'runModule' at application startup.
--
-- [@s@] The nested options of next module in stack. Options are layered the
-- similar way as parts of monad transformers.
data ModuleSampleOptions s = ModuleSampleOptions {
  moduleSampleOptsNext   :: s -- ^ Nested options of next game module
}

-- | Internal environment of game module
data ModuleSampleEnv t = ModuleSampleEnv {
  -- | Options that were used to create the module
  moduleSampleEnvOptions    :: ModuleSampleOptions ()
}

-- | Create a new environment for game module
newModuleSampleEnv :: MonadAppHost t m => ModuleSampleOptions s -> m (ModuleSampleEnv t)
newModuleSampleEnv opts = do
  return ModuleSampleEnv {
      moduleSampleEnvOptions = opts { moduleSampleOptsNext = () }
    }

-- | Implementation of 'MonadModuleSample' API.
--
-- [@t@] FRP engine, you could ignore this parameter as it resolved only at main
-- function of your application.
--
-- [@m@] Underlying game modules, next layer in monad stack.
--
-- [@a@] Result of computation.
--
-- How to embed the monad into your app:
--
-- @
-- type AppStack t = ModuleSampleT t (LoggingT t (TimerT t (GameMonad t)))
--
-- newtype AppMonad t a = AppMonad { runAppMonad :: AppStack t a}
--   deriving (Functor, Applicative, Monad, MonadFix)
-- @
--
-- And you will need some boilerplate code for instance deriving, see
-- `examples/Example01.hs` for full example.
--
newtype ModuleSampleT t m a = ModuleSampleT { runModuleSampleT :: ReaderT (ModuleSampleEnv t) m a }
  deriving (Functor, Applicative, Monad, MonadReader (ModuleSampleEnv t), MonadFix
    , MonadIO, MonadThrow, MonadCatch, MonadMask, MonadSample t, MonadHold t)

instance {-# OVERLAPPING #-} MonadAppHost t m => MonadModuleSample t (ModuleSampleT t m) where
  exampleFunc = return ()
  {-# INLINE exampleFunc #-}

-- Boilerplate

instance MonadTrans (ModuleSampleT t) where
  lift = ModuleSampleT . lift

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (ModuleSampleT t m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (ModuleSampleT t m) where
  subscribeEvent = lift . subscribeEvent

instance MonadAppHost t m => MonadAppHost t (ModuleSampleT t m) where
  getFireAsync = lift getFireAsync
  getRunAppHost = do
    runner <- ModuleSampleT getRunAppHost
    return $ \m -> runner $ runModuleSampleT m
  performPostBuild_ = lift . performPostBuild_
  liftHostFrame = lift . liftHostFrame

instance MonadTransControl (ModuleSampleT t) where
  type StT (ModuleSampleT t) a = StT (ReaderT (ModuleSampleEnv t)) a
  liftWith = defaultLiftWith ModuleSampleT runModuleSampleT
  restoreT = defaultRestoreT ModuleSampleT

instance MonadBase b m => MonadBase b (ModuleSampleT t m) where
  liftBase = ModuleSampleT . liftBase

instance (MonadBaseControl b m) => MonadBaseControl b (ModuleSampleT t m) where
  type StM (ModuleSampleT t m) a = ComposeSt (ModuleSampleT t) m a
  liftBaseWith     = defaultLiftBaseWith
  restoreM         = defaultRestoreM

instance (MonadIO (HostFrame t), GameModule t m) => GameModule t (ModuleSampleT t m) where
  type ModuleOptions t (ModuleSampleT t m) = ModuleSampleOptions (ModuleOptions t m)
  runModule opts (ModuleSampleT m) = do
    s <- newModuleSampleEnv opts
    runModule (moduleSampleOptsNext opts) $ runReaderT m s
  withModule t _ = withModule t (Proxy :: Proxy m)

