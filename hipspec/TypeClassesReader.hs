{-# LANGUAGE RankNTypes, ScopedTypeVariables, DeriveDataTypeable, FlexibleContexts #-}

module TypeClassesReader where

import HipSpec
import qualified Prelude
import Data.Typeable

data Nat = Z | S Nat
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord, Typeable)

id :: a -> a
id x = x

(.)    :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

($) :: (a -> b) -> a -> b
f $ x = f x

const x _ = x

data Identity a = Identity { runIdentity :: a } deriving (Prelude.Show, Prelude.Eq, Prelude.Ord, Typeable)

fmapIdentity :: (a -> b) -> Identity a -> Identity b
fmapIdentity f (Identity x) = Identity (f x) 

identReturn = Identity

identMonad :: Identity a -> (a -> Identity b) -> Identity b
m `identMonad` k  = k (runIdentity m)

data ReaderT r a = ReaderT { runReaderT :: r -> Identity a } deriving (Typeable)

type Reader r = ReaderT r

instance Names Nat where names _ = ["n0", "n1", "n2"]
instance Names (Identity a) where names _ = ["i0", "i1", "i2"]
instance Names (ReaderT r a) where names _ = ["r0", "r1", "r2"]

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = sized (\ len -> do { gen })
        where 
            gen :: Gen (Identity a)
            gen = do
                x <- arbitrary
                Prelude.return (Identity x)

instance (CoArbitrary r, Arbitrary a) => Arbitrary (ReaderT r a) where
    arbitrary = do
                f <- arbitrary
                Prelude.return (ReaderT f)

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

instance CoArbitrary Nat where
    coarbitrary Z = variant 0
    coarbitrary (S n) = variant 1 Prelude.. coarbitrary n

instance CoArbitrary Identity where
    coarbitrary Identity = variant 0

-- | Runs a @Reader@ and extracts the final value from it.
-- (The inverse of 'reader'.)
runReader
    :: Reader r a       -- ^ A @Reader@ to run.
    -> r                -- ^ An initial environment.
    -> a
runReader m = runIdentity . runReaderT m
{-# INLINE runReader #-}

-- | Transform the computation inside a @ReaderT@.
--
-- * @'runReaderT' ('mapReaderT' f m) = f . 'runReaderT' m@
mapReaderT :: (Identity a -> Identity b) -> ReaderT r a -> ReaderT r b
mapReaderT f m = ReaderT $ (f . runReaderT m)
{-# INLINE mapReaderT #-}

fmap :: (Nat -> Nat) -> Reader Nat Nat -> Reader Nat Nat
fmap f  = mapReaderT (fmapIdentity f )

pure :: a -> Reader r a
pure    = liftReaderT . identReturn

(<**>) :: Identity (a -> b) -> Identity a -> Identity b
(<**>) f x = Identity ((runIdentity f) (runIdentity x))

(<*>) :: Reader r (a1 -> a2) -> Reader r a1 -> Reader r a2
f <*> v = ReaderT $ (\ r -> runReaderT f r <**> runReaderT v r)

return   = liftReaderT . identReturn

m >>= k  = ReaderT $ (\r -> do
    runReaderT m r `identMonad` (\a -> runReaderT (k a) r))

liftReaderT :: Identity a -> ReaderT r a
liftReaderT m = ReaderT (const m)
{-# INLINE liftReaderT #-}

-- fmapId :: Nat -> Reader Nat Nat -> Prop Nat
-- fmapId s xs = runReader (fmap id xs) s =:= runReader (id xs) s

-- fmapComposition :: r -> (b -> c) -> (a -> b) -> Reader r a -> Equation
-- fmapComposition s f g xs = runReaderT (fmap (f . g) xs) s =:= runReaderT ((fmap f . fmap g) xs) s

-- appIdentity :: r -> Reader r a -> Equation
-- appIdentity s v = runReaderT (pure id <*> v) s =:= runReaderT v s

-- appComposition :: r -> Reader r (a1 -> b) -> Reader r (a2 -> a1) -> Reader r a2 -> Equation
-- appComposition s u v w = runReaderT (pure (.) <*> u <*> v <*> w) s =:= runReaderT (u <*> (v <*> w)) s

-- appHomomorphism :: forall r a b . r -> (a -> b) -> a -> Equation
-- appHomomorphism s f x = runReaderT (pure f <*> (pure :: a -> Reader r a) x) s =:= runReaderT (pure (f x)) s

-- appInterchange :: r -> Reader r (a -> b) -> a -> Equation
-- appInterchange s u y = runReaderT (u <*> pure y) s =:= runReaderT (pure ($ y) <*> u) s

-- monadLeftIdentity :: r -> a -> (a -> Reader r b) -> Equation
-- monadLeftIdentity s a k = runReaderT (return a >>= k) s === runReaderT (k a) s

-- monadRightIdentity :: r -> Reader r b -> Equation
-- monadRightIdentity s m = runReaderT (m >>= return) s =:= runReaderT m s

-- monadAssociativity :: r -> Reader r a1 -> p -> (a1 -> Reader r a2) -> (a2 -> Reader r b) -> Equation
-- monadAssociativity s m x k h = runReaderT (m >>= (\x -> k x >>= h)) s =:= runReaderT ((m >>= k) >>= h) s
