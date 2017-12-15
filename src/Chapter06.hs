module Chapter06 where

-- https://www.reddit.com/r/HaskellBook/comments/79a3su/ch_6_on_a_scale_of_1_to_stupid/
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b
-- chk f a b = (==) (f a) b
-- chk f a = (==) (f a)
-- chk f a = (==) . f $ a
-- chk f = (==) . f
-- chk f = (.) (==) f
-- chk = (.) (==)

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f n a = f a + fromInteger n
