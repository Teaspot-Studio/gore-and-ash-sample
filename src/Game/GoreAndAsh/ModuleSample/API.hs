{-|
Module      : Game.GoreAndAsh.ModuleSample.API
Description : API of resource module
Copyright   : (c) Anton Gushcha, 2016-2017
                  Anatoly Nardid, 2016-2017
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module contains public API for sample module.
-}
module Game.GoreAndAsh.ModuleSample.API(
    MonadModuleSample(..)
  ) where

import Control.Monad.Trans
import Game.GoreAndAsh

-- | Public API of resouce module.
--
-- You can use like a mtl type class:
--
-- @
-- foo :: (MonadModuleSample t m, LoggingMonad t m) => m ()
-- @
class MonadAppHost t m => MonadModuleSample t m | m -> t where
  exampleFunc :: m ()

instance {-# OVERLAPPABLE #-} (MonadTrans mt, MonadAppHost t (mt m), MonadModuleSample t m)
  => MonadModuleSample t (mt m) where

  exampleFunc = lift exampleFunc
  {-# INLINE exampleFunc #-}
