{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE OverlappingInstances     #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE EmptyDataDecls           #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Test where

import Generics.Regular
import Generics.Regular.Functions.Arbitrary
import Generics.Regular.Functions.Binary

import Test.QuickCheck (quickCheck, sized, elements, generate, Gen)
import qualified Test.QuickCheck as Q (Arbitrary(..))
import Data.Binary.Put (runPut)
import Data.Binary.Get (runGet)
import System.Random (newStdGen)
import Data.List (sort, group)


-- Datatype representing logical expressions
data Logic = Var String
           | Logic :->:  Logic  -- implication
           | Logic :<->: Logic  -- equivalence
           | Logic :&&:  Logic  -- and (conjunction)
           | Logic :||:  Logic  -- or (disjunction)
           | Not Logic          -- not
           | T                  -- true
           | F                  -- false
           deriving (Eq, Show)

-- Instantiating Regular for Logic using TH
$(deriveAll ''Logic "PFLogic")
type instance PF Logic = PFLogic

-- Simple datatype for testing data generation
data Choice = A1 | A2 | A3 | A4
  deriving (Show, Eq, Ord)

-- Instantiating Regular for Choice using TH
$(deriveAll ''Choice "PFChoice")
type instance PF Choice = PFChoice

----------------------------------------------------------------------------
-- Testing arbitrary
ex1 = do 
        let c arb = newStdGen >>= \r -> return $ generate 123 r arb
            arb1, arb2 :: Gen Choice
            arb1 = arbitrary
            arb2 = sized (arbitraryWith [("A1",1),("A2",1),("A3",3),("A4",5)])
            pp c = sequence (take 10000 (repeat c)) >>=
                     return . map (\x -> (head x, length x)) . group . sort
        c1 <- pp (c arb1)
        c2 <- pp (c arb2)
        putStrLn ("Normal arbitrary: " ++ show c1)
        putStrLn ("Custom arbitrary: " ++ show c2)

-- Note that we use overlapping instances for this
instance Arbitrary (K String) where
  harbitrary _ _ _ _ = return $ fmap K $ elements ["p", "q"]

ex2 = do
        let c arb = newStdGen >>= \r -> return $ generate 123 r arb
            arbShort, arbLong :: Gen Logic
            arbShort = arbitraryWith [("Var", 3),("T", 3),("F", 3)] 3
            arbLong  = arbitraryWith [] 20
        c1 <- c arbShort
        c2 <- c arbLong
        putStrLn ("Short expression: " ++ show c1)
        putStrLn ("Long expression: " ++ show c2)

-- Testing binary
instance Q.Arbitrary Choice where
  arbitrary = arbitrary

instance Q.Arbitrary Logic where
  arbitrary = arbitrary

-- To keep the compiler happy
instance Q.Arbitrary Char

-- Testing that deserializing after serializing is the identity
ex3 = let propC :: Choice -> Bool
          propC x = runGet gget (runPut (gput x)) == x
          propL :: Logic -> Bool
          propL x = runGet gget (runPut (gput x)) == x
      in quickCheck propC >> quickCheck propL
