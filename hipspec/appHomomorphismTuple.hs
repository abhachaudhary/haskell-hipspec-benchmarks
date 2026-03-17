{-# LANGUAGE RankNTypes, ScopedTypeVariables, DeriveDataTypeable #-}

module TypeClassesTuple where

import HipSpec
import qualified Prelude 
import Data.Typeable

data Tuple a b = Tuple a b deriving (Prelude.Show, Prelude.Eq, Prelude.Ord, Typeable)

data Nat
  = Z
  | S Nat
  deriving (Prelude.Show, Prelude.Eq, Prelude.Ord, Typeable)

(+) :: Nat -> Nat -> Nat
Z + y = y
(S x) + y = S (x + y)

data List a = a :> List a | Nil deriving (Prelude.Show, Prelude.Eq, Prelude.Ord, Typeable)
infixl 4 :>

instance Names Nat where names _ = ["n0", "n1", "n2"]
instance Names (List a) where names _ = ["l0", "l1", "l2"]
instance Names (Tuple a b) where names _ = ["t0", "t1", "t2"]

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

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = sized (\ len -> do { x <- choose (0, len); gen x })
        where gen :: Prelude.Int -> Gen (List a)
              gen 0 = Prelude.return Nil
              gen n = sized (\s -> do
                x <- arbitrary
                tail <- gen (n Prelude.- 1)
                Prelude.return (x :> tail))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b) where
    arbitrary = sized (\ len -> do {  gen })
        where gen :: Gen (Tuple a b)
              gen = do
                x <- arbitrary
                y <- arbitrary
                Prelude.return (Tuple x y)

instance CoArbitrary Nat where
    coarbitrary Z = variant 0
    coarbitrary (S n) = variant 1 Prelude.. coarbitrary n

(++) :: List a -> List a -> List a
(++) Nil     ys = ys
(++) (x :> xs) ys = x :> (xs ++ ys)

foldr :: (a -> b -> b) -> b -> List a -> b
foldr k z Nil     = z
foldr k z (y :> ys) = y `k` foldr k z ys


(<>) :: Tuple Nat Nat -> Tuple Nat Nat -> Tuple Nat Nat
Tuple x1 y1 <> Tuple x2 y2 = Tuple (x1 + x2) (y1 + y2)

mempty = Tuple Z Z

mconcat :: List (Tuple Nat Nat) -> Tuple Nat Nat
mconcat = foldr (<>) mempty

fmap f (Tuple x y) = Tuple x (f y)

id :: a -> a
id x = x

(.)    :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)



-- -- Applicative laws
pure x = Tuple Z x

($) :: (a -> b) -> a -> b
f $ x = f x

(<*>) :: Tuple Nat (a -> b) -> Tuple Nat a -> Tuple Nat b
Tuple x1 f <*> Tuple x2 y = Tuple (x1 + x2) (f y)



appHomomorphism :: (Nat -> Nat) -> Nat -> Prop (Tuple Nat Nat)
appHomomorphism f x = (pure f <*> (pure :: Nat -> Tuple Nat Nat) x) =:= pure (f x)