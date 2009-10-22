{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE TypeOperators            #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Functions.Binary
-- Copyright   :  (c) 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Generic Data.Binary instances.
--
-- These generic functions can be used to create a "Data.Binary" instance. For
-- example, for a user-defined type @MyType@, the following code is necessary:
--
-- > import Data.Binary
-- > import Generics.Regular.Base
-- > import Generics.Regular.Binary
-- >
-- > data MyType = ...
-- >
-- > $(deriveAll ''MyType "PFMyType")
-- > type instance PF MyType = PFMyType
-- >
-- > instance Binary MyType where
-- >   put = gput
-- >   get = gget
--
-----------------------------------------------------------------------------

module Generics.Regular.Functions.Binary (
    
    -- * Binary put and get
    GBinary, gput, gget
    
  ) where

import Control.Applicative
import Generics.Regular.Base
import qualified Data.Binary as B

-- * Generic Data.Binary instances.

class GBinary f where
  hput :: (r -> B.Put)   -> f r -> B.Put
  hget :: (     B.Get r) ->        B.Get (f r)

instance GBinary I where
  hput f (I x) = f x
  hget f       = I <$> f

instance B.Binary a => GBinary (K a) where
  hput _ (K x) = B.put x
  hget _       = K <$> B.get

instance GBinary U where
  hput _ _ = B.put ()
  hget _   = return U

instance (GBinary f, GBinary g) => GBinary (f :+: g) where
  hput t (L x) = B.put True  >> hput t x
  hput t (R y) = B.put False >> hput t y
  hget t       = B.get >>= \v -> if v then L <$> hget t else R <$> hget t

instance (GBinary f, GBinary g) => GBinary (f :*: g) where
  hput t (x :*: y) = hput t x >> hput t y
  hget t           = (:*:) <$> hget t <*> hget t

instance GBinary f => GBinary (C c f) where
  hput t (C x) = hput t x
  hget t       = C <$> hget t

instance GBinary f => GBinary (S s f) where
  hput t (S x) = hput t x
  hget t       = S <$> hget t

-- | Generic binary @put@ to be used with "Data.Binary.Put".

gput :: (Regular a, GBinary (PF a)) => a -> B.Put
gput p = hput (\q -> gput q) (from p)

-- | Generic binary @get@ to be used with "Data.Binary.Get".

gget :: (Regular a, GBinary (PF a)) => B.Get a
gget = to <$> hget gget
