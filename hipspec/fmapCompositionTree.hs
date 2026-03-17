{-# LANGUAGE RankNTypes, ScopedTypeVariables, DeriveDataTypeable #-}

module TypeClassesTree where

import HipSpec
import qualified Prelude
import Data.Typeable

data Nat = Z | S Nat
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord, Typeable)
data Tree a = Node (Tree a) a (Tree a) | Leaf deriving (Prelude.Show, Prelude.Eq, Prelude.Ord, Typeable)

instance Names Nat where names _ = ["n0", "n1", "n2"]
instance Names (Tree a) where names _ = ["l0", "l1", "l2"]

instance Prelude.Enum Nat where
    toEnum 0 = Z
    toEnum n = S (Prelude.toEnum (n Prelude.- 1))

    fromEnum Z = 0
    fromEnum (S n) = Prelude.fromEnum n Prelude.+ 1

arbitraryEnum :: (Prelude.Enum a, Arbitrary a) => Gen a
arbitraryEnum = sized (\ s -> do
    x <- choose (0, s)
    Prelude.return (Prelude.toEnum x))

instance Arbitrary Nat where arbitrary = arbitraryEnum

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized (\ len -> do { x <- choose (0, len); gen x })
        where gen :: Prelude.Int -> Gen (Tree a)
              gen 0 = Prelude.return Leaf
              gen n = sized (\s -> do
                node_val <- arbitrary
                left <- gen (n `Prelude.div` 2)
                right <- gen (n `Prelude.div` 2)
                Prelude.return (Node left node_val right))

instance CoArbitrary Nat where
    coarbitrary Z = variant (0 :: Prelude.Int)
    coarbitrary (S n) = variant (1 :: Prelude.Int) Prelude.. coarbitrary n

id :: a -> a
id x = x

(.)    :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

fmap :: (a -> b) -> Tree a -> Tree b
fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)
fmap f Leaf = Leaf

pure :: a -> Tree a
pure x = Node (pure x) x (pure x)
    
(<*>) :: Tree (a -> b) -> Tree a -> Tree b
Leaf <*> _ = Leaf
_ <*> Leaf = Leaf
Node l1 f1 r1 <*> Node l2 x2 r2 = Node (l1 <*> l2) (f1 x2) (r1 <*> r2)
  
-- Functor laws

fmapComposition :: (Nat -> Nat) -> (Nat -> Nat) -> Tree Nat -> Prop (Tree Nat)
fmapComposition f g xs = fmap (f . g) xs =:= (fmap f . fmap g) xs