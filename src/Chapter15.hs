{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Chapter15
  ( Combine(..)
  , Optional(Nada,Only)
  , XOR(..)
  ) where

import Data.Semigroup (Semigroup((<>)))
import Test.QuickCheck (Arbitrary(arbitrary), CoArbitrary, frequency)
import Test.SmallCheck.Series ((\/), Serial(series), cons0, cons1, newtypeCons)

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

newtype XOR = XOR Bool
  deriving (Eq, Show)

newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
    Combine f <> Combine g = Combine (f <> g)

----------------------------------------------------------------------
-- Monoid instances

instance Monoid a => Monoid (Optional a) where
    mempty = Nada

    mappend Nada x = x
    mappend x Nada = x
    mappend (Only a) (Only b) = Only (mappend a b)

instance Monoid XOR where
    mempty = XOR False
    mappend (XOR a) (XOR b) = XOR (a /= b)

----------------------------------------------------------------------
-- Arbitrary instances

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = frequency [ (1, pure Nada)
                          , (3, Only <$> arbitrary) ]

instance Arbitrary XOR where
    arbitrary = XOR <$> arbitrary

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = Combine <$> arbitrary

----------------------------------------------------------------------
-- Serial instances

instance Serial m a => Serial m (Optional a) where
    series = cons0 Nada \/ cons1 Only

instance Monad m => Serial m XOR where
    series = newtypeCons XOR
