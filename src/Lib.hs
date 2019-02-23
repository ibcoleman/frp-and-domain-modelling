module Lib
    ( someFunc
    ) where

import Data.Fixed
import Data.Time
import Control.Applicative(liftA2)
type Amount = Centi
type Dollars = Integer
type Cents = Integer
type Error = String


x :: Centi
x = MkFixed 1000

-- The account type
data Account = Account { acctno :: String
                       , name :: String
                       , opendt :: Day
                       , balance :: Amount }
             deriving (Eq, Show)


-- Smart constructor for an Amount
toAmount :: Dollars -> Cents -> Either Error Amount
toAmount d c
  | d < 0 = Left "Dollars was less than zero."
  | c > 99 || c < 0 = Left "Cents was less than zero or more than 99."
  | otherwise = Right $ MkFixed (d * 100 + c) 
  
-- The Debit function 
-- Create an account
acct = Right Account { acctno = "8239322"
                       , name = "Bob Userman"
                       , opendt = fromGregorian 2010 10 23
                       , balance = MkFixed 10000}


debit :: Account -> Amount -> Account
debit (Account a n o b) amt = Account a n o (b - amt)

liftedDebit :: Either Error Account -> Either Error Amount -> Either Error Account
liftedDebit eAcc eAm = debit <$> eAcc <*> eAm

otherLiftedDebit :: Either Error Account -> Either Error Amount -> Either Error Account
otherLiftedDebit = liftA2 debit


-- Executing --
{-

debit takes an Either Error Account -> Amount  and returns an Either Error Account.
 - foldMap show acct - shows an Account from a Maybe Account
 - 
-}


-- Just the thing that kicks it off
someFunc :: IO ()
someFunc = 
  putStrLn $ show $ liftA2 debit acct (toAmount 23 33)

