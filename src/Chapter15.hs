{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Chapter15
  ( XOR(..)
  ) where

import Test.SmallCheck.Series (Serial, newtypeCons, series)

newtype XOR = XOR Bool
  deriving (Eq, Show)

instance Monad m => Serial m XOR where
    series = newtypeCons XOR

xor :: Bool -> Bool -> Bool
xor = (/=)

instance Monoid XOR where
    mempty = XOR False
    mappend (XOR a) (XOR b) = XOR (xor a b)
