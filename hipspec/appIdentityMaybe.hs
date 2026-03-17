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
instance Names (Maybe a) where names _ = ["m0", "m1", "m2"]

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

data Maybe a  =  Nothing | Just a deriving (Prelude.Show, Prelude.Eq, Prelude.Ord, Typeable)

instance Arbitrary a => Arbitrary (Maybe a) where 
    arbitrary = sized (\ len -> do { x <- choose (0, 1); gen x })
        where 
            gen :: Prelude.Int -> Gen (Maybe a)
            gen 0 = Prelude.return Nothing
            gen 1 = do
                x <- arbitrary
                Prelude.return (Just x)

id :: a -> a
id x = x

-- Applicative laws
pureList :: (Nat -> Nat) -> Maybe (Nat -> Nat)
pureList x    = Just x

fmap :: (Nat -> Nat) -> Maybe Nat -> Maybe Nat
fmap _ Nothing       = Nothing
fmap f (Just a)      = Just (f a)

(<*>) :: Maybe (Nat -> Nat) -> Maybe Nat -> Maybe Nat
Just f  <*> m       = fmap f m
Nothing <*> _m      = Nothing

appIdentity :: Maybe Nat -> Prop (Maybe Nat)
appIdentity v = (pureList id <*> v) =:= v
