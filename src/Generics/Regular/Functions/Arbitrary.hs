{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE OverlappingInstances     #-}
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
    FrequencyTable, Arbitrary(..), arbitraryWith, arbitrary,
    
    -- * Generic coarbitrary functionality
    CoArbitrary(..), corbitrary
  
  ) where

import Generics.Regular.Functions.Fixpoints
import Generics.Regular.Functions.ConNames
import Generics.Regular.Base

import Test.QuickCheck (Gen, frequency, sized, variant)
import qualified Test.QuickCheck as Q (Arbitrary, arbitrary, coarbitrary)
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
class Arbitrary f where 
  harbitrary :: (Int -> Gen a) -> FrequencyTable -> Int -> Int
             -> Maybe (Gen (f a))

instance (Fixpoints f, Fixpoints g, ConNames f, Arbitrary f,
            ConNames g, Arbitrary g) => Arbitrary (f :+: g) where
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

instance (Arbitrary f, Constructor c) => Arbitrary (C c f) where
  harbitrary r ft m n = fmap (fmap C) (harbitrary r ft m n)
                       
instance Arbitrary I where
  harbitrary r _ m n = return $ fmap I $ r (n `div` m)

instance Arbitrary U where
  harbitrary _ _ _ _ = return $ return U

instance (Q.Arbitrary a) => Arbitrary (K a) where
  harbitrary _ _ _ _ = return $ fmap K $ Q.arbitrary

instance (Arbitrary f, Arbitrary g) => Arbitrary (f :*: g) where
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
arbitraryWith :: (Regular a, Arbitrary (PF a))
           => FrequencyTable -> Int -> Gen a
arbitraryWith ft = fmap to . fromJust . harbitrary (arbitraryWith ft) ft 1

-- | Generic arbitrary function with default sizes and constructor frequencies.
arbitrary :: (Regular a, Arbitrary (PF a)) => Gen a
arbitrary = sized (arbitraryWith [])


-- | Generic CoArbitrary class
class CoArbitrary f where 
  hcoarbitrary :: (b -> Gen a -> Gen a) -> Int -> f b -> Gen a -> Gen a

instance (CoArbitrary f, CoArbitrary g, ConNames g)
            => CoArbitrary (f :+: g) where
  hcoarbitrary r n (L x) = hcoarbitrary r n x
  hcoarbitrary r n (R x) = hcoarbitrary r (n + length (hconNames x)) x

instance (CoArbitrary f, Constructor c) => CoArbitrary (C c f) where
  hcoarbitrary r n (C x) = variant n . hcoarbitrary r n x

instance CoArbitrary I where
  hcoarbitrary r _ (I x) = r x

instance CoArbitrary U where
  hcoarbitrary _ _ _ = id

instance (Q.Arbitrary a) => CoArbitrary (K a) where
  hcoarbitrary _ _ (K a) = Q.coarbitrary a

instance (CoArbitrary f, CoArbitrary g) => CoArbitrary (f :*: g) where
  hcoarbitrary r n (x1 :*: x2) = hcoarbitrary r n x1 . hcoarbitrary r n x2

-- | Generic coarbitrary function.
corbitrary :: (Regular b, CoArbitrary (PF b))
           => b -> Gen a -> Gen a
corbitrary = hcoarbitrary corbitrary 0 . from
