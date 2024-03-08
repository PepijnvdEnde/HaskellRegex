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

isOdd :: Int -> Bool
isOdd a -> a mod 2 == 1 -> True

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = isPrime' n 2
    where
        isPrime' n i
            | i == n = True
            | n mod i == 0 = False
            | otherwise = isPrime' n (i + 1)

