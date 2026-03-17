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

-- Applicative laws
pureList x    = x :> Nil

-- GHC's base defines (<*>) on lists as:
-- @ fs <*> xs = [f x | f <- fs, x <- xs] @
-- which does not work with the custom list type we need for CycleQ.
-- The below is the result of compiling this to G2's intermediate representation,
-- lambda lifting let-bound functions, and then adapting to work with List.

-- With build in list type:
-- @
-- (<**>) :: forall a . forall b . [a -> b] -> [a] -> [b]
-- f <**> xs = ds'2 f xs

-- ds'2 ds1'2 xs = case ds1'2 of
--             []  -> []
--             ds3 : ds4 -> ds5 xs ds3 ds4 xs

-- ds5 ds6 ds3 ds4 xs = case ds6 of
--             []  -> ds'2 ds4 xs
--             ds8 : ds9 -> (ds3 ds8):(ds5 ds9 ds3 ds4 xs)
-- @

-- Testing with built in list type:
--      ghci> ([\x -> x + 1, \y -> y * 2, \z -> z + 7] <*> [1..10]) == ([\x -> x + 1, \y -> y * 2, \z -> z + 7] <**> [1..10])
--      True
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

-- Definition found following the same method as (<*>)
-- ghci> ([1..10] >>= (\y -> [y + 1, y * 2, y + 18])) == ([1..10] >>== (\y -> [y + 1, y * 2, y + 18]))
-- True
(>>=) :: List a -> (a -> List b) -> List b
(>>=) xs'7 f'118 = ds'37 xs'7 f'118

ds'37 :: List t -> (t -> List a) -> List a
ds'37 ds1'28 f'118 = case ds1'28 of
            Nil  -> Nil
            ds3'5 :> ds4'4 -> ds5'4 (f'118 ds3'5) f'118 ds4'4

ds5'4 :: List a -> (t -> List a) -> List t -> List a
ds5'4 ds6'4 f'118 ds4'4 = case ds6'4 of
                        Nil  -> ds'37 ds4'4 f'118
                        ds8'3 :> ds9'3 -> ds8'3:>(ds5'4 ds9'3 f'118 ds4'4)

return :: a -> List a
return x = x :> Nil

monadLeftIdentity :: Nat -> (Nat -> List Nat) -> Prop (List Nat)
monadLeftIdentity a k = (return a >>= k) =:= k a