{-# LANGUAGE RankNTypes, ScopedTypeVariables, DeriveDataTypeable #-}

module TypeClassesFunction where

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

fmap :: (Nat -> Nat) -> (Nat -> Nat) -> Nat -> Nat
fmap = (.)

(.)    :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

id :: a -> a
id x = x

const                   :: a -> b -> a
const x _               =  x

($) :: (a -> b) -> a -> b
f $ x = f x

mempty = id

foldr :: (a -> b -> b) -> b -> List a -> b
foldr k z Nil     = z
foldr k z (y :> ys) = y `k` foldr k z ys

mconcat = foldr (.) mempty

pure :: a -> Nat -> a
pure = const

(<*>) :: (Nat -> a -> b) -> (Nat -> a) -> Nat -> b
(<*>) f g x = f x (g x)

return = pure

f >>= k = \ r -> k (f r) r

monadAssociativity :: (Nat -> Nat) -> Nat -> (Nat -> (Nat -> Nat)) -> (Nat -> (Nat -> Nat)) -> Prop (Nat -> Nat)
monadAssociativity m x k h = (m >>= (\x -> k x >>= h)) =:= ((m >>= k) >>= h)