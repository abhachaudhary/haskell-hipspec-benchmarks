{-# LANGUAGE RankNTypes, ScopedTypeVariables, DeriveDataTypeable #-}

module TypeClassesList where

import HipSpec
import qualified Prelude
import Data.Typeable

data Nat = Z | S Nat
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord, Typeable)

data List a = a :> List a | Nil deriving (Prelude.Show, Prelude.Eq, Prelude.Ord, Typeable)
infixl 4 :>

instance Names Nat where names _ = ["n0", "n1", "n2"]
instance Names (List a) where names _ = ["l0", "l1", "l2"]

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

instance CoArbitrary Nat where
    coarbitrary Z = variant 0
    coarbitrary (S n) = variant 1 Prelude.. coarbitrary n

(++) :: List a -> List a -> List a
(++) Nil     ys = ys
(++) (x :> xs) ys = x :> (xs ++ ys)

memptyList :: List Nat
memptyList = Nil

mconcatList :: List (List a) -> List a
mconcatList Nil = Nil
mconcatList (xs :> xss) = xs ++ mconcatList xss

foldr :: (a -> b -> b) -> b -> List a -> b
foldr k z Nil     = z
foldr k z (y :> ys) = y `k` foldr k z ys

map :: (Nat -> Nat) -> List Nat -> List Nat
map _ Nil     = Nil
map f (x:>xs) = f x :> map f xs

fmap = map

id :: a -> a
id x = x

(.)    :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

-- Semigroup laws
semigroupAssociativity :: List Nat -> List Nat -> List Nat -> Prop (List Nat)
semigroupAssociativity x y z = x ++ (y ++ z) =:= (x ++ y) ++ z

monoidRightIdentity :: List Nat -> Prop (List Nat)
monoidRightIdentity x = x ++ memptyList =:= x

monoidLeftIdentity :: List Nat -> Prop (List Nat)
monoidLeftIdentity x = memptyList ++ x =:= x 

monoidConcatenation :: List (List Nat) -> Prop (List Nat)
monoidConcatenation xs = mconcatList xs =:= foldr (++) memptyList xs

-- Applicative laws
pureList x    = x :> Nil


(<*>) :: List (a -> b) -> List a -> List b
f <*> xs = ds'2 f xs

ds'2 :: List (t -> a) -> List t -> List a
ds'2 ds1'2 xs = case ds1'2 of
            Nil  -> Nil
            ds3 :> ds4 -> ds5 xs ds3 ds4 xs

ds5 :: List t -> (t -> a) -> List (t -> a) -> List t -> List a
ds5 ds6 ds3 ds4 xs = case ds6 of
            Nil  -> ds'2 ds4 xs
            ds8 :> ds9 -> (ds3 ds8):>(ds5 ds9 ds3 ds4 xs)

($) :: (a -> b) -> a -> b
f $ x = f x

appInterchange :: List (Nat -> Nat) -> Nat -> Prop (List Nat)
appInterchange u y = (u <*> pureList y) =:= (pureList ($ y) <*> u)