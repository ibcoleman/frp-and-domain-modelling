module Lib
    ( someFunc
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
getDayFromUTCTime :: UTCTime -> Day
getDayFromUTCTime (UTCTime day time) = day
  
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



-------
{-
  Let's experiment: Get an IO String from console. Write a simple function to make
  sure it's got at least 4 characters. If it does, return an Maybe Just String that says "Hi".
  If not return a Nothing.
-}

sayHello :: String -> IO ()
sayHello name =
  putStrLn ("Hi " ++ name ++ "!")

sayInteger :: Integer -> IO ()
sayInteger number =
  putStrLn ("Hi " ++ (show number) ++ "!")

isLongEnough :: String  -> String
isLongEnough s
  | length s > 4 = s
  | otherwise = "error"


today :: IO Day
today = getDayFromUTCTime <$> getCurrentTime

-- mkDay :: String -> String -> String -> Day
-- mkDay y m d = do
--   year  <- read y
--   month <- fromIntegral $ read $ m
--   day   <- fromIntegral $ read $ d
--   fromGregorian year month day

getYear :: String -> Maybe Integer
getYear yearStr = readMaybe yearStr

getIntFromString :: String -> Maybe Int
getIntFromString dayStr = readMaybe dayStr

-- Ok, ignore IO for a bit; let's new up a Maybe Day and another Maybe Day
-- Compare two Day values
-- Compare a Maybe Day value and a Day value
-- Compare two Maybe Day values
-- Compare an IO Day and a Maybe Day



-- This is the signature for chaining IO actions:
--     (>>) :: IO a -> IO b -> IO b


-- skunk = do
--   day  <- (fromGregorianValid 2018 10 23)
--  Here day is type Maybe Day
--  sayHello <$> show <$> day


test :: IO ()
test = do
  putStr "Enter day: "
  day        <- readMaybe <$> getLine  --  Here Day 
  
  putStr "Enter month: "
  month      <- readMaybe <$> getLine
  
  putStr "Enter year: "
  year       <- readMaybe <$> getLine

  -- At this point day, month, year are all `IO (Maybe Int)`
  -- fromGregorianValid :: Integer -> Int -> Int -> Maybe Day
  -- <$> takes a function that is  a -> b and applies it to an f a (year) and results in a f (a -> b)
  -- So... given fromGregorianValid's signature, partially applying year you'll get
  --     apply year, and you get Maybe (Int -> Int -> Maybe Day)
  --     apply month with <*> because now the functions's not Int -> Int -> Maybe Day anymore, it's Maybe (* -> * -> *)
  --          and you get Maybe (Int -> Maybe Day)
  --     apply day as "Maybe (Int -> Maybe Day) -> Maybe Int -> Maybe (Maybe Day)
  --          and you get Maybe (Maybe Day).
  -- Call join $ Maybe (Maybe Day) and you've got (Maybe Day)
  -- Call pure . (Maybe Day) and you've got IO (Maybe Day) which print can print via print :: a -> IO ()
  -- Print 
  maybeDay    <- pure $ join $ fromGregorianValid <$> year <*> month <*> day
  systemDay   <- pure <$> today
  dayDiff     <- pure $ diffDays <$> maybeDay <*> systemDay
  outString   <- pure $ dayDiff
  print outString
  
  
testTwo :: IO ()
testTwo = do
  putStr "Enter day: "
  day        <- readMaybe <$> getLine  --  Here Day 
  
  putStr "Enter month: "
  month      <- readMaybe <$> getLine

  putStr "Enter year: "
  year       <- readMaybe <$> getLine

  systemDay  <- pure <$> today      -- apply pure to the thing inside the context `(IO Day) -> IO (f Day)` later on this'll be `IO (Maybe Day)`

  let
    userDay    = join $ fromGregorianValid <$> year <*> month <*> day
    daysDiff   = diffDays <$> userDay <*> systemDay
    outString  = daysDiff
    
  print outString
  
  
str2bool :: String -> Bool
str2bool s = length s > 5
