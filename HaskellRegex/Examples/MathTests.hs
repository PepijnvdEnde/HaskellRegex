module Main (main) where

--run all the functions in the main function`
main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn "The 5th fibonacci number is: "
  print(fibonacci 5)
  putStrLn "The factorial of 5 is: "
  print(factorial 5)
  putStrLn "The golden ratio is: "
  print goldenRatio
  putStrLn "Is 5 a prime number? "
  print(isPrime 5)
  putStrLn "The greatest common divisor of 5 and 10 is: "
  print(greatestCommonDivisor 5 10)
  putStrLn "The smallest common multiple of 30 and 20 is: "
  print(smallestCommonMultiple 30 20)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n - 2) + fibonacci(n - 1)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n - 1)

goldenRatio :: Float
goldenRatio = (1 + sqrt 5)/2

isPrime :: Int -> Bool
isPrime n  | n < 2 = False
           | otherwise = null [ x |  x <- [2.. isqrt n], n `mod` x == 0]
      where isqrt = truncate . (sqrt :: Double -> Double) . fromIntegral
  
greatestCommonDivisor :: Int -> Int -> Int
greatestCommonDivisor a 0 = a 
greatestCommonDivisor a b = greatestCommonDivisor b (a `mod` b)

smallestCommonMultiple :: Int -> Int -> Int
smallestCommonMultiple a b = a * b `div` greatestCommonDivisor a b


