{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE ScopedTypeVariables      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Functions.Fixpoints
-- Copyright   :  (c) 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Auxiliary module for "Generics.Regular.Functions.Fixpoints".
-----------------------------------------------------------------------------

module Generics.Regular.Functions.Fixpoints (

    Fixpoints(..), fixpoints,
    Tree(..), foldTree, sumTree
    
  ) where

import Generics.Regular.Base


-- | Tree structure to store fixed points as found in the data type.
data Tree a = Leaf a | Node (Tree a) (Tree a)
 deriving Show

foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree l _ (Leaf x)    = l x
foldTree l n (Node x y)  = (foldTree l n x) `n` (foldTree l n y)

sumTree :: Tree Int -> Int
sumTree = foldTree id (+)

-- | The class to compute fixed points.
class Fixpoints f where 
    hFixpoints :: f a -> Tree Int

instance (Fixpoints f, Fixpoints g) => Fixpoints (f :+: g) where
    hFixpoints (_ :: (f :+: g) a) = 
      Node (hFixpoints (undefined :: f a))
           (hFixpoints (undefined :: g a))
    
instance (Fixpoints f, Constructor c) => Fixpoints (C c f) where
    hFixpoints (_ :: (C c f) a) = hFixpoints (undefined :: f a)

instance (Fixpoints f, Fixpoints g) => Fixpoints (f :*: g) where
    hFixpoints (_ :: (f :*: g) a) = 
      let Leaf m = hFixpoints (undefined :: f a)
          Leaf n = hFixpoints (undefined :: g a)
      in Leaf (m + n)

instance Fixpoints I where
    hFixpoints _ = Leaf 1

instance Fixpoints U where
    hFixpoints _ = Leaf 0

instance Fixpoints (K a) where
    hFixpoints _ = Leaf 0

-- | Return a tree structure of the fixed points of a datatype
fixpoints :: (Regular a, Fixpoints (PF a)) => a -> Tree Int
fixpoints x = hFixpoints (undefined `asTypeOf` (from x))
