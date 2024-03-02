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

(<+>) :: Regexp -> Regexp -> Regexp -- union
Zero <+> e = e
e <+> Zero = e
e1 <+> e2  = Plus e1 e2

(<>) :: Regexp -> Regexp -> Regexp -- concatenation
Zero <> _   = Zero
_ <> Zero   = Zero
One <> e    = e
e <> One    = e
e1 <> e2    = Cat e1 e2

many :: Regexp -> Regexp  -- repetition
many Zero     = One
many One       = One
many (Many e)  = Many e
many e         = Many e

type Cont= String -> Bool

accept :: Regexp -> String -> Cont -> Bool  -- accept a string for a regular expression with a continuation 
accept Zero    cs      k = False
accept One     cs      k = k cs
accept (Lit c) (c':cs) k = c==c' && k cs
accept (Lit c) []      k = False
accept (Cat e1 e2) cs  k = accept e1 cs (\cs' -> accept e2 cs' k)
accept (Plus e1 e2) cs k = accept e1 cs k || accept e2 cs k
accept (Many e) cs k     = acceptMany e cs k
  where 
     acceptMany e cs k 
       = k cs || accept e cs (\cs' -> cs'/=cs && acceptMany e cs' k)


match :: Regexp -> String -> Bool -- match a string for a regular expression
match re s = accept re s null

instance IsString Regexp where
  fromString cs = foldr ((<>) . Lit) One cs

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
