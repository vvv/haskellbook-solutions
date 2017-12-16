module Chapter15
  ( Optional(Nada,Only)
  , XOR(..)
  ) where

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada

    mappend Nada x = x
    mappend x Nada = x
    mappend (Only a) (Only b) = Only (mappend a b)

newtype XOR = XOR Bool
  deriving (Eq, Show)

xor :: Bool -> Bool -> Bool
xor = (/=)

instance Monoid XOR where
    mempty = XOR False
    mappend (XOR a) (XOR b) = XOR (xor a b)
