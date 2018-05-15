{-# LANGUAGE InstanceSigs                    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Chapter22 where

import Control.Applicative (liftA2)
import Data.Char (toUpper)

boop = (*2)

doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop = fmap boop doop

bbop = (+) <$> boop <*> doop

duwop = liftA2 (+) boop doop

-- (<$>) :: (a -> a -> a) -> (a -> a) -> (a -> a -> a)
-- (<*>) :: (a -> a -> b) -> (a -> a) -> (a -> b)
--
-- bbop 3
-- ((+) <$> (*2) <*> (+10)) 3
-- (3*2) + (3+10)
-- (+ (* 2 3) (+ 10 3))

boopDoop = do
    a <- boop
    b <- doop
    pure (a + b)

-- So, we’ve seen here that we can have a Functor, Applicative, and
-- Monad for partially applied functions. In all cases, these are
-- awaiting application to one argument that will allow both functions
-- to be evaluated. The Functor of functions is function composition.
-- The Applicative and Monad chain the argument forward in addition to
-- the composition (applicatives and monads are both varieties of
-- functors, so they retain that core functorial behavior).
--
-- This is the idea of Reader. It is a way of stringing functions
-- together when all those functions are awaiting one input from a
-- shared environment. [...] the important intuition here is that it’s
-- another way of abstracting out function application and gives us a
-- way to do computation in terms of an argument that hasn’t been
-- supplied yet. We use this most often when we have a constant value
-- that we will obtain from somewhere outside our program that will be
-- an argument to a whole bunch of functions. Using Reader allows us
-- to avoid passing that argument around explicitly.

cap :: String -> String
cap = map toUpper

rev :: [a] -> [a]
rev = reverse

composed :: String -> String
composed = rev . cap

fmapped :: String -> String
fmapped = rev <$> cap

tupled :: String -> (String, String)
tupled = (,) <$> composed <*> fmapped
-- tupled = liftA2 (,) composed fmapped

tupledM :: String -> (String, String)
tupledM = do
    a <- composed
    b <- fmapped
    pure (a, b)

-- We use function composition because it lets us compose two func-
-- tions without explicitly having to recognize the argument that will
-- eventually arrive; the Functor of functions is function composition.
-- With the Functor of functions, we are able to map an ordinary
-- function over another to create a new function awaiting a final
-- argument. The Applicative and Monad instances for the function type
-- give us a way to map a function that is awaiting an `a` over
-- another function that is also awaiting an `a`.
--
-- Giving it a name helps us know the what and why of what we’re
-- doing: reading an argument from the environment into functions.

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader ra) = Reader (f . ra)

ask :: Reader a a
ask = Reader id

-- f ~ ((->) r)
-- pure :: a -> (r -> a)
-- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure = Reader . const

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    -- Reader rab <*> Reader ra = Reader (\r -> (rab r) (ra r))
    -- Reader rab <*> Reader ra = Reader (($) <$> rab <*> ra)
    -- Reader rab <*> Reader ra = Reader (liftA2 ($) rab ra)
    f <*> v = Reader (runReader f <*> runReader v)
    -- rab :: r -> a -> b
    -- ra :: r -> a

newtype HumanName = HumanName String
  deriving (Eq, Show)

newtype DogName = DogName String
  deriving (Eq, Show)

newtype Address = Address String
  deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Str.")

chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
-- getDogR = Dog <$> dogName <*> address
getDogR = liftA2 Dog dogName address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader
