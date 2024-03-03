module Main (main) where

import Data.List (isInfixOf, elemIndices)

main :: IO ()
main = print (regexMatch "(The)(color)" "The color green")

regexMatch :: String -> String -> Bool
regexMatch regex input | regex == input = True
                       | containsSubstring regex input = True
                       | containsSubstring regex input /= extractAndMatchSubstring regex input = True
                       | otherwise = False

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

extractAndMatchSubstring :: String -> String -> Bool
extractAndMatchSubstring regex input =
    let substrings = extractSubstrings regex
    in all (`containsSubstring` input) substrings
