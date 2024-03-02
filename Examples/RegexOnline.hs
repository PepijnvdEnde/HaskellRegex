{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Prelude hiding ((<>))
import GHC.Exts (IsString(..))

data Regexp = Zero                  -- empty
            | One                   -- epsilon
            | Lit Char              -- single character
            | Plus Regexp Regexp    -- union (+)
            | Cat  Regexp Regexp    -- concatenation (.)
            | Many Regexp           -- repetition (*)
            deriving Show

infixl 6 <+>
infixl 7 <>

-- union is that when we have two regular expressions e1 and e2, the union e1 + e2 matches a string if either e1 or e2 does.
(<+>) :: Regexp -> Regexp -> Regexp -- union
Zero <+> e = e
e <+> Zero = e
e1 <+> e2  = Plus e1 e2
-- e means empty string, so e + e = e

-- concatenation is that when we have two regular expressions e1 and e2, the concatenation e1 . e2 matches a string if e1 matches the beginning of the string and e2 matches the rest of the string.
(<>) :: Regexp -> Regexp -> Regexp -- concatenation
Zero <> _   = Zero
_ <> Zero   = Zero
One <> e    = e
e <> One    = e
e1 <> e2    = Cat e1 e2
-- e means empty string, so e . e = e

many :: Regexp -> Regexp  -- repetition
many Zero     = One
many One       = One
many (Many e)  = Many e
many e         = Many e
-- e means empty string, so e* = e

type Cont= String -> Bool

accept :: Regexp -> String -> Cont -> Bool  -- accept a string for a regular expression with a continuation
accept Zero string       k = False
accept One     string      k = k string
accept (Lit char) (char':string) k = char==char' && k string
accept (Lit char) []      k = False
accept (Cat e1 e2) string  k = accept e1 string (\string' -> accept e2 string' k)
accept (Plus e1 e2) string k = accept e1 string k || accept e2 string k
accept (Many e) string k     = acceptMany e string k
  where
     acceptMany e string k
       = k string || accept e string (\string' -> string'/=string && acceptMany e string' k)
-- e means empty string, so e* = e
-- c means a character
-- cs means a string
-- k means a continuation can also be call
-- Cont means a continuation


match :: Regexp -> String -> Bool -- match a string for a regular expression
match re s = accept re s null
-- re means regular expression
-- s means a string
-- null means empty string

instance IsString Regexp where
  fromString cs = foldr ((<>) . Lit) One cs
-- cs means a string

main :: IO ()
main = do
    -- Matching a single character
    print (match (Lit 'a') "a")
    print (not (match (Lit 'a') "b"))

    -- Matching concatenation
    print (match ("hello " <> "world") "hello world")
    print (not (match ("hello" <> "world") "worldhello"))

    -- Matching union
    print (match ("apple" <+> "banana") "apple")
    print (match ("apple" <+> "banana") "banana")
    print (not (match ("apple" <+> "banana") "orange"))

    -- Matching repetition
    print (match (many "ab") "ababab")
    print (match (many "ab") "ab")
    print (not (match (many "ab") "baba"))

    -- Testing combination of features
    print (match ("abb" <> ("a" <+>  many "b")) "abbbbb")
    print (match ("a" <> many "b" <+> "c") "c")
    print (not (match ("ab" <> many "ba" <> "c") "a"))
