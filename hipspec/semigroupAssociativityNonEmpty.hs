{-# LANGUAGE RankNTypes, ScopedTypeVariables, DeriveDataTypeable #-}

module TypeClassesNonEmpty where

import HipSpec
import qualified Prelude 
import Data.Typeable

data Nat = Z | S Nat
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord, Typeable)

data List a = a :> List a | Nil
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord, Typeable)
infixl 4 :>

data NonEmpty a = a :| List a
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord, Typeable)

infixr 5 :|

instance Names Nat where names _ = ["n0", "n1", "n2"]
instance Names (List a) where names _ = ["l0", "l1", "l2"]
instance Names (NonEmpty a) where names _ = ["ne0", "ne1", "ne2"]

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

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = sized (\ len -> do { x <- choose (0, len); gen x })
        where gen :: Prelude.Int -> Gen (NonEmpty a)
              gen n = sized (\s -> do
                x <- arbitrary
                tail <- arbitrary
                Prelude.return (x :| tail))

instance CoArbitrary Nat where
    coarbitrary Z = variant 0
    coarbitrary (S n) = variant 1 Prelude.. coarbitrary n

(++) :: List a -> List a -> List a
(++) Nil     ys = ys
(++) (x :> xs) ys = x :> (xs ++ ys)

(a :| as) <> ~(b :| bs) = a :| (as ++ (b :> bs))

mapList :: (Nat -> Nat) -> List Nat -> List Nat
mapList _ Nil     = Nil
mapList f (x:>xs) = f x :> mapList f xs

fmap :: (Nat -> Nat) -> NonEmpty Nat -> NonEmpty Nat
fmap f (a :| as) = f a :| mapList f as

id :: a -> a
id x = x

(.)    :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

-- Semigroup laws
semigroupAssociativity :: NonEmpty Nat -> NonEmpty Nat -> NonEmpty Nat -> Prop (NonEmpty Nat)
semigroupAssociativity x y z = x <> (y <> z) =:= (x <> y) <> z