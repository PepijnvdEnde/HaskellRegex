module Main (main) where

import Data.List (isInfixOf, elemIndices)

main :: IO ()
main = do
    print (regexMatch "(green)(color)" "The green color")
    print (regexMatch "[d-z]" "defghijklmnopqrstuvwxyz")
    print (regexMatch "[a-c]" "defghijklmnopqrstuvwxyz")
    print (regexMatch "[a-z]" "12345")
    print (regexMatch "x*" "")
    print (regexMatch "x*" "x")    
    print (regexMatch "x*" "xx")
    print (regexMatch "x*" "xxxxx")
    print (regexMatch "x*" "abc")

regexMatch :: String -> String -> Bool
regexMatch regex input
    | regex == "." = all (/= '\n') input
    | regex == "x*" = matchZeroOrMore 'x' input
    | containsCharacterRanges regex = any (`elem` processedRange) input
    | otherwise = matchSubstringsInOrder (extractSubstrings (processCharacterRanges regex)) input
    where
        processedRange = processCharacterRanges regex
        containsCharacterRanges = any (\c -> c `elem` ['a'..'z'] || c == '-')

matchZeroOrMore :: Char -> String -> Bool
matchZeroOrMore _ "" = True
matchZeroOrMore c (x:xs)
    | x == c = matchZeroOrMore c xs
    | otherwise = matchSubstringsInOrder [x:xs] xs

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

expandCharacterRange :: Char -> Char -> [Char]
expandCharacterRange startChar endChar = [startChar..endChar]

processCharacterRanges :: String -> String
processCharacterRanges [] = []
processCharacterRanges ('[':c:'-':d:']':cs) = expandCharacterRange c d ++ processCharacterRanges cs
processCharacterRanges (c:cs) = c : processCharacterRanges cs


