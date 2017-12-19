{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Chapter15
  ( Optional(Nada,Only)
  , XOR(..)
  ) where

import Test.SmallCheck.Series ((\/), Serial, cons0, cons1, newtypeCons, series)

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada

    mappend Nada x = x
    mappend x Nada = x
    mappend (Only a) (Only b) = Only (mappend a b)

instance Serial m a => Serial m (Optional a) where
    series = cons0 Nada \/ cons1 Only

newtype XOR = XOR Bool
  deriving (Eq, Show)

instance Monad m => Serial m XOR where
    series = newtypeCons XOR

xor :: Bool -> Bool -> Bool
xor = (/=)

instance Monoid XOR where
    mempty = XOR False
    mappend (XOR a) (XOR b) = XOR (xor a b)
