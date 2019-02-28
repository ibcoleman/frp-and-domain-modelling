module Lib
    (
      Amount,
      Dollars,
      Cents,
      Error,
      Account,
      toAmount,
      toAccount,
      debit
    ) where

import Data.Fixed
import Data.Time
import Control.Applicative(liftA2, liftA3)
import Control.Monad(join)
import Text.Read(readMaybe)


-- aliases
type Amount = Centi
type Dollars = Integer
type Cents = Integer
type Error = String


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

-- Smart constructor for an Account
toAccount :: String -> String -> Day -> Amount -> Either Error Account
toAccount acctno name opendt bal
  | acctno == "" = Left "Account Number Can't Be Blank."
  | name   == "" = Left "Name Can't Be Blank."
--  | opendt - check if it's in the past
  | otherwise = Right (Account acctno name opendt bal)

-- get Day from a UTCTime value. If we want to do this "inline" you can do like:
--     `UTCTime day time <- getCurrentTime`.
-- that'll bind the result of getCurrentTime to day/time via destructuring...
-- 
-- To apply this following function you'd do either:
--     fmap getDayFromUTCTime getCurrentTime
-- or
--     getCurrentTime <$> getDayFromUTCTime
--
-- The Debit function 
-- Create an account
acct = Right Account { acctno = "8239322"
                       , name = "Bob Userman"
                       , opendt = fromGregorian 2010 10 23
                       , balance = MkFixed 10000}


debit :: Account -> Amount -> Account
debit (Account a n o b) amt = Account a n o (b - amt)

-- liftedDebit :: Either Error Account -> Either Error Amount -> Either Error Account
-- liftedDebit eAcc eAm = debit <$> eAcc <*> eAm

-- otherLiftedDebit :: Either Error Account -> Either Error Amount -> Either Error Account
-- otherLiftedDebit = liftA2 debit


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
  -- we  could also do this as putStrLn $ show $ debit <$> acct <*> (toAmount 23 33)



