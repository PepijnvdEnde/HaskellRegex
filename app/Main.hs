module Main (main) where

import Data.List (isInfixOf, elemIndices)

main :: IO ()
main = print (regexMatch "(color)" "The color green")

regexMatch :: String -> String -> Bool
regexMatch regex input | regex == input = True
                       | containsSubstring regex input = True
                       | containsSubstring regex input /= extractAndMatchSubstring regex input = True
                       | otherwise = False

containsSubstring :: String -> String -> Bool
containsSubstring substring string = substring `isInfixOf` string

extractAndMatchSubstring :: String -> String -> Bool
extractAndMatchSubstring regex input 
    | containsSubstring "(" regex && containsSubstring ")" regex = 
        let start = head (elemIndices '(' regex) + 1
            end = head (elemIndices ')' regex)
            substring = take (end - start) (drop start regex)
        in containsSubstring substring input
    | otherwise = False
