{-# OPTIONS_GHC -Wall -Werror #-}
module UT (assertEq) where

assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq name expected actual =
    if actual == expected
    then putStrLn $ name ++ " OK"
    else do
        putStrLn $ name ++ " **ERROR**"
        putStrLn $ "| expected " ++ show expected
        putStrLn $ "|      got " ++ show actual
