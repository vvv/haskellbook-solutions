module Chapter16
  ( GoatLord(NoGoat,OneGoat,MoreGoats)
  , e_ch16
  ) where

import Control.Applicative (liftA3)
import Test.QuickCheck (Arbitrary(arbitrary), oneof)

data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat x) = OneGoat (f x)
    fmap f (MoreGoats x y z) = MoreGoats (f <$> x) (f <$> y) (f <$> z)

instance Arbitrary a => Arbitrary (GoatLord a) where
    arbitrary = oneof
      [ pure NoGoat
      , OneGoat <$> arbitrary
      , liftA3 (\x y z -> MoreGoats x y z) arbitrary arbitrary arbitrary
      ]

e_ch16 :: IO Integer
e_ch16 = let ioi = readIO "1" :: IO Integer
             changed = read . ("123" ++) . show <$> ioi
         in (*3) <$> changed
