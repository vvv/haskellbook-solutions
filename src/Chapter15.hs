{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Chapter15
  ( Optional(Nada,Only)
  , XOR(..)
  ) where

import Test.QuickCheck (Arbitrary(arbitrary), frequency)
import Test.SmallCheck.Series ((\/), Serial(series), cons0, cons1, newtypeCons)

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada

    mappend Nada x = x
    mappend x Nada = x
    mappend (Only a) (Only b) = Only (mappend a b)

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = frequency [ (1, pure Nada)
                          , (3, Only <$> arbitrary) ]

instance Serial m a => Serial m (Optional a) where
    series = cons0 Nada \/ cons1 Only

newtype XOR = XOR Bool
  deriving (Eq, Show)

instance Arbitrary XOR where
    arbitrary = XOR <$> arbitrary

instance Monad m => Serial m XOR where
    series = newtypeCons XOR

instance Monoid XOR where
    mempty = XOR False
    mappend (XOR a) (XOR b) = XOR (a /= b)
