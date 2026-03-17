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


-- -- Applicative laws
pureNonEmpty x    = x :| Nil

($) :: (a -> b) -> a -> b
f $ x = f x

(<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
m1 <*> m2 = (>>==) m1 (\x1 -> (>>==) m2 (\x2 -> (x1 x2) :| Nil))

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

return :: a -> NonEmpty a
return x = x :| Nil

(>>==) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
(a :| as) >>== f = case f a of
                        b :| bs -> b :| (bs ++ (as >>= ((\(c :| cs) -> c :> cs) . f)))


monadRightIdentity :: NonEmpty Nat -> Prop (NonEmpty Nat)
monadRightIdentity m = (m >>== return) =:= m