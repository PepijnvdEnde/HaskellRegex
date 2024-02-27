module Main (main) where

--run all the functions in the main function`
main :: IO ()
main = print(isEven 2)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n - 2) + fibonacci(n - 1)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n - 1)

goldenRatio :: Float
goldenRatio = (1 + sqrt 5)/2

isEven :: Int -> Bool
isEven a -> a mod 2 == 0 -> True

