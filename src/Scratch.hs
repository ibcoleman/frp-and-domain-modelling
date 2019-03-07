module Scratch () where

import GHC.Arr
import qualified Data.Map.Strict as Map
import Data.List.Split

data BoolAndSomethingElse a =
  False' a | True' a deriving (Show, Eq)

instance Functor BoolAndSomethingElse where
  fmap f (False' x) = False' (f x)
  fmap f (True' x)  = True' (f x)
  
------------------------------------------------------------------------
data BoolAndMaybeSomethingElse a =
  Falsish | Trueish a deriving (Show, Eq)

instance Functor BoolAndMaybeSomethingElse where
  fmap f Falsish = Falsish
  fmap f (Trueish x) = Trueish (f x)

------------------------------------------------------------------------
data Sum b a =
  First a | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b  

------------------------------------------------------------------------
data Company a c b =
  DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

------------------------------------------------------------------------
data More b a =
  L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'


---------------
{-
  Given a map of morse codes:
  - How about parsing the raw string into a [[String, String, String], [String, String]]?

 Spec:
  - One (or more) spaces is a break between chars; 3 (or more) spaces between words

-}

morseStrings = Map.fromList [(".-","A"), ("-...","B"), ("-.-.", "C"), ("-..", "D")]

wordDelim   = "   "
letterDelim = " "

mToChar :: String -> String
mToChar str = morseStrings Map.! str

decodeMorse :: String -> [String]
decodeMorse morseStr = charsToWords chars
  where
    words = splitOn wordDelim morseStr    -- [String]
    chars = fmap (splitOn " ") words      -- [[String]]

charsToWords :: [[String]] -> [String]
charsToWords (morseStrings : xs)
  | xs == []    = mLettersToAWord morseStrings : []
  | otherwise   = mLettersToAWord morseStrings : charsToWords xs

mLettersToAWord :: [String] -> String
mLettersToAWord mLetters = foldl (++) [] $ fmap mToChar mLetters
    



{-
".- -... -.-.   .- -... -.-."
-}
