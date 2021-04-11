{-|
Module      : Foreign.Storable.Generic
Copyright   : (c) Mateusz Kłoczko, 2016
License     : MIT
Maintainer  : mateusz.p.kloczko@gmail.com
Stability   : experimental
Portability : portable



-}

{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE UndecidableInstances #-}


module Foreign.Storable.Generic (GStorable (..), Storable(..), getFilling) where



import Foreign.Storable (Storable(..))
import GHC.Generics (Generic(..))

import Foreign.Storable.Generic.Internal (GStorable (..), GStorable'(..))
import Foreign.Storable.Generic.Instances
import qualified Foreign.Storable.Generic.Tools as Tools


------Association to Storable class-------

instance {-# OVERLAPS #-} (GStorable a) => (Storable a) where
    {-# INLINE sizeOf #-}
    sizeOf      = gsizeOf
    {-# INLINE alignment #-}
    alignment   = galignment
    {-# INLINE peekByteOff #-}
    peekByteOff = gpeekByteOff
    {-# INLINE pokeByteOff #-}
    pokeByteOff = gpokeByteOff


-- | A helper to visualize layout.
getFilling :: (Generic a, GStorable a, GStorable' (Rep a)) => a -> [Tools.Filling]
getFilling x = Tools.getFilling $ zip sizes aligns
    where sizes  = glistSizeOf'    gx
          aligns = glistAlignment' gx
          gx = from x
