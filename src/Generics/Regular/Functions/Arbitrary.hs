{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeOperators            #-}

{-# OPTIONS_GHC -fno-warn-orphans     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Functions.Arbitrary
-- Copyright   :  (c) 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic "Test.QuickCheck" instances.
-----------------------------------------------------------------------------

module Generics.Regular.Functions.Arbitrary (
  
    -- * Generic arbitrary functionality
    FrequencyTable, GArbitrary(..), garbitraryWith, garbitrary,
    
    -- * Generic coarbitrary functionality
    GCoArbitrary(..), gcoarbitrary
  
  ) where

import Generics.Regular.Functions.Fixpoints
import Generics.Regular.Functions.ConNames
import Generics.Regular.Base

import Test.QuickCheck (Arbitrary, arbitrary, Gen, frequency, coarbitrary,
                        sized, variant)
import Data.Maybe (fromJust)

-- | A frequency table detailing how often certain constructors should be
-- picked. The 'String' corresponds to the constructor name, as returned by
-- 'Generics.Regular.Functions.ConNames.conNames'.
type FrequencyTable = [(String,Int)]

frequencies :: [String] -> FrequencyTable -> Int
frequencies [] _ = 0
frequencies (s:ss) ft = let freqs = case lookup s ft of
                                      Just f  -> f
                                      Nothing -> 1
                        in freqs + frequencies ss ft

-- | Generic Arbitrary class
class GArbitrary f where 
  harbitrary :: (Int -> Gen a) -> FrequencyTable -> Int -> Int
             -> Maybe (Gen (f a))

instance (Fixpoints f, Fixpoints g, ConNames f, GArbitrary f,
            ConNames g, GArbitrary g) => GArbitrary (f :+: g) where
  harbitrary r ft _ n = 
    let (Node ff fg)   = hFixpoints (undefined :: (f :+: g) a)
        fConNames      = hconNames (undefined :: f a)
        gConNames      = hconNames (undefined :: g a)
        fFrequency     = calcFreq n (sumTree ff) (frequencies fConNames ft)
        gFrequency     = calcFreq n (sumTree fg) (frequencies gConNames ft)
        calcFreq 0 0 _ = 1
        calcFreq 0 _ _ = 0
        calcFreq _ _ d = d
        rl = maybe [] (\x -> [(fFrequency,fmap L x)]) 
               (harbitrary r ft (sumTree ff) n)
        rr = maybe [] (\x -> [(gFrequency,fmap R x)])
               (harbitrary r ft (sumTree fg) n)
    in if null (rl ++ rr) then Nothing else return $ frequency $ rl ++ rr

instance (GArbitrary f, Constructor c) => GArbitrary (C c f) where
  harbitrary r ft m n = fmap (fmap C) (harbitrary r ft m n)
                       
instance GArbitrary I where
  harbitrary r _ m n = return $ fmap I $ r (n `div` m)

instance GArbitrary U where
  harbitrary _ _ _ _ = return $ return U

instance (Arbitrary a) => GArbitrary (K a) where
  harbitrary _ _ _ _ = return $ fmap K $ arbitrary

instance (GArbitrary f, GArbitrary g) => GArbitrary (f :*: g) where
  harbitrary r ft m n = do rl <- harbitrary r ft m n
                           rr <- harbitrary r ft m n
                           return $ do
                             x <- rl
                             y <- rr
                             return (x :*: y)

-- | Generic arbitrary function, sized and with custom constructor frequencies.

-- This function does not require any particular
-- nesting order of the sums of the generic representation, but it does require
-- every constructor to be properly tagged with C. Representations generated
-- with the supplied Template Haskell code are compliant.
garbitraryWith :: (Regular a, GArbitrary (PF a))
           => FrequencyTable -> Int -> Gen a
garbitraryWith ft = fmap to . fromJust . harbitrary (garbitraryWith ft) ft 1

-- | Generic arbitrary function with default sizes and constructor frequencies.
garbitrary :: (Regular a, GArbitrary (PF a)) => Gen a
garbitrary = sized (garbitraryWith [])


-- | Generic CoArbitrary class
class GCoArbitrary f where 
  hcoarbitrary :: (b -> Gen a -> Gen a) -> Int -> f b -> Gen a -> Gen a

instance (GCoArbitrary f, GCoArbitrary g, ConNames g)
            => GCoArbitrary (f :+: g) where
  hcoarbitrary r n (L x) = hcoarbitrary r n x
  hcoarbitrary r n (R x) = hcoarbitrary r (n + length (hconNames x)) x

instance (GCoArbitrary f, Constructor c) => GCoArbitrary (C c f) where
  hcoarbitrary r n (C x) = variant n . hcoarbitrary r n x

instance GCoArbitrary I where
  hcoarbitrary r _ (I x) = r x

instance GCoArbitrary U where
  hcoarbitrary _ _ _ = id

instance (Arbitrary a) => GCoArbitrary (K a) where
  hcoarbitrary _ _ (K a) = coarbitrary a

instance (GCoArbitrary f, GCoArbitrary g) => GCoArbitrary (f :*: g) where
  hcoarbitrary r n (x1 :*: x2) = hcoarbitrary r n x1 . hcoarbitrary r n x2

-- | Generic coarbitrary function.
gcoarbitrary :: (Regular b, GCoArbitrary (PF b))
           => b -> Gen a -> Gen a
gcoarbitrary = hcoarbitrary gcoarbitrary 0 . from
