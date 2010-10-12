{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Functions.Seq
-- Copyright   :  (c) 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Deep generic seq. Used to fully evaluate a term.
-----------------------------------------------------------------------------

module Generics.Regular.Functions.Seq (
  
    Seq(..), gdseq
  
  ) where

import Control.DeepSeq
import Generics.Regular.Base

-- | The class for generic deep seq.
class Seq f where
  gseq :: (a -> b -> b) -> f a -> b -> b

instance Seq I where
  gseq f (I x) = f x

-- | For constants we rely on the |DeepSeq| class.
instance (NFData a) => Seq (K a) where
  gseq _ (K x) = deepseq x
  
instance Seq U where
  gseq _ U = id

instance (Seq f, Seq g) => Seq (f :+: g) where
  gseq f (L x) = gseq f x
  gseq f (R y) = gseq f y

instance (Seq f, Seq g) => Seq (f :*: g) where
  gseq f (x :*: y) = gseq f x . gseq f y

instance Seq f => Seq (C c f) where
  gseq f (C x) = gseq f x

instance Seq f => Seq (S s f) where
  gseq f (S x) = gseq f x


-- | Deep, generic version of seq.
gdseq :: (Regular a, Seq (PF a)) => a -> b -> b
gdseq p = gseq gdseq (from p)
