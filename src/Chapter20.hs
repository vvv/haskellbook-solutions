module Chapter20 where

import Data.Monoid ((<>), Sum(..), getSum)

-- instance Foldable t where
--     foldr :: (a -> b -> b) -> b -> t a -> b
--     foldMap :: Monoid m => (a -> m) -> t a -> m

data Constant a b = Constant b

instance Foldable (Constant a) where
    foldr f z (Constant x) = f x z
    foldMap f (Constant x) = f x

data Two a b = Two a b

instance Foldable (Two a) where
    foldr f z (Two _ x) = f x z
    foldMap f (Two _ x) = f x

data Three a b c = Three a b c

instance Foldable (Three a b) where
    foldr f z (Three _ _ x) = f x z
    foldMap f (Three _ _ x) = f x

data Three' a b = Three' a b b

instance Foldable (Three' a) where
    foldr f z (Three' _ x y) = f y (f x z)
    foldMap f (Three' _ x y) = f x <> f y

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
    foldMap f (Four' _ x y z) = f x <> f y <> f z

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- foldMap' f = foldr (\x z -> f x <> z) mempty
foldMap' f = foldr ((<>) . f) mempty

filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF p xs =
    let f x | p x = pure x
            | otherwise = mempty
    in foldMap f xs
