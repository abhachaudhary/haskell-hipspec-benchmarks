{-# LANGUAGE RankNTypes, ScopedTypeVariables, DeriveDataTypeable #-}

module TypeClassesMaybe where

import HipSpec
import qualified Prelude 
import Data.Typeable

data Nat = Z | S Nat
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord, Typeable)

data Maybe a  =  Nothing | Just a deriving (Prelude.Show, Prelude.Eq, Prelude.Ord, Typeable)

data List a = a :> List a | Nil deriving (Prelude.Show, Prelude.Eq, Prelude.Ord, Typeable)
infixl 4 :>

instance Names Nat where names _ = ["n0", "n1", "n2"]
instance Names (List a) where names _ = ["l0", "l1", "l2"]
instance Names (Maybe a) where names _ = ["m0", "m1", "m2"]
instance Names D where names _ = ["d0", "d1", "d2"]

instance Prelude.Enum Nat where
    toEnum 0 = Z
    toEnum n = S (Prelude.toEnum (n Prelude.- 1))
    fromEnum Z = 0
    fromEnum (S n) = Prelude.fromEnum n Prelude.+ 1

instance Prelude.Enum a => Prelude.Enum (Maybe a) where
    toEnum 0 = Nothing
    toEnum n = Just (Prelude.toEnum (n Prelude.- 1))
    fromEnum Nothing = 0
    fromEnum (Just a) = Prelude.fromEnum a Prelude.+ 1

arbitraryEnum :: (Prelude.Enum a, Arbitrary a) => Gen a
arbitraryEnum = sized (\ s -> do
    x <- choose (0, s)
    Prelude.return (Prelude.toEnum x))

instance Arbitrary Nat where arbitrary = arbitraryEnum
instance Arbitrary a => Arbitrary (Maybe a) where 
    arbitrary = sized (\ len -> do { x <- choose (0, 1); gen x })
        where 
            gen :: Prelude.Int -> Gen (Maybe a)
            gen 0 = Prelude.return Nothing
            gen 1 = do
                x <- arbitrary
                Prelude.return (Just x)
              

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

instance CoArbitrary a => CoArbitrary (Maybe a) where
    coarbitrary Nothing = variant 0
    coarbitrary (Just x) = variant 1 Prelude.. coarbitrary x

-- instance CoArbitrary (Maybe D) where
--     coarbitrary Nothing = variant 0
--     coarbitrary (Just x) = variant 1 Prelude.. coarbitrary x

instance CoArbitrary D where
    coarbitrary D = variant 0

(++) :: List a -> List a -> List a
(++) Nil     ys = ys
(++) (x :> xs) ys = x :> (xs ++ ys)

foldr :: (a -> b -> b) -> b -> List a -> b
foldr k z Nil     = z
foldr k z (y :> ys) = y `k` foldr k z ys

data D = D deriving (Prelude.Show, Prelude.Eq, Prelude.Ord, Typeable)

instance Arbitrary D where 
    arbitrary = sized (\ len -> do { gen })
        where gen :: Gen D
              gen = Prelude.return D


(<|>) :: D -> D -> D
D <|> D = D

(<>) :: Maybe D -> Maybe D -> Maybe D
Nothing <> b       = b
a       <> Nothing = a
Just a  <> Just b  = Just (a <|> b)

mempty :: Maybe D
mempty = Nothing

mconcat :: List (Maybe D) -> Maybe D
mconcat = foldr (<>) mempty

fmap _ Nothing       = Nothing
fmap f (Just a)      = Just (f a)

id :: a -> a
id x = x

(.)    :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

-- -- Semigroup laws
semigroupAssociativity :: Maybe D -> Maybe D -> Maybe D -> Prop (Maybe D)
semigroupAssociativity x y z = x <> (y <> z) =:= (x <> y) <> z