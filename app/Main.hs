module Main (main) where

import Data.List (isInfixOf, elemIndices)

main :: IO ()
main = print (regexMatch "(green)(color)" "The green color")

regexMatch :: String -> String -> Bool
regexMatch regex input
    | regex == "." = all (/= '\n') input
    | otherwise = matchSubstringsInOrder (extractSubstrings regex) input

matchSubstringsInOrder :: [String] -> String -> Bool
matchSubstringsInOrder [] _ = True
matchSubstringsInOrder _ "" = False
matchSubstringsInOrder (x:xs) str =
    (x `isInfixOf` str) && matchSubstringsInOrder xs (dropWhile (/= ' ') (drop (length x) str))

containsSubstring :: String -> String -> Bool
containsSubstring substring string = substring `isInfixOf` string

extractSubstrings :: String -> [String]
extractSubstrings regex
    | not (containsSubstring "(" regex) || not (containsSubstring ")" regex) = []
    | otherwise =
        let start = head (elemIndices '(' regex) + 1
            end = head (elemIndices ')' regex)
            substring = take (end - start) (drop start regex)
        in substring : extractSubstrings (drop (end + 1) regex)