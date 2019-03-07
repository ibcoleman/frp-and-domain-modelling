module Date
  (
    runDateDemo
  ) where

import System.IO
import Data.Time
import Control.Monad(join)
import Control.Applicative(liftA2)
import Text.Read(readMaybe)
import Lib(toAccount, toAmount, Error)

-- Use decomposition to get the Day from the UTCTime
getDayFromUTCTime :: UTCTime -> Day
getDayFromUTCTime (UTCTime day time) = day

-- Return today's Day from the system clock.
today :: IO Day
today = getDayFromUTCTime <$> getCurrentTime


-- We use this function to go from a Maybe Day returned by Data.Time to a Either Error Day
eitherDay :: Maybe a -> Either Error a
eitherDay (Just a) = Right a
eitherDay Nothing  = Left "That was not a valid day!"


-- 
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
  

-- This is what the main function calls.   
runDateDemo :: IO ()
runDateDemo = do
  putStr "Enter day: "
  hFlush stdout
  day        <- readMaybe <$> getLine  --  Here Day 
  
  putStr "Enter month: "
  hFlush stdout
  month      <- readMaybe <$> getLine

  putStr "Enter year: "
  hFlush stdout
  year       <- readMaybe <$> getLine

  systemDay  <- pure <$> today      -- apply pure to the thing inside the context `(IO Day) -> IO (f Day)` later on this'll be `IO (Maybe Day)`

  let
    userDay    = join $ fromGregorianValid <$> year <*> month <*> day
    daysDiff   = diffDays <$> userDay <*> systemDay
    outString  = daysDiff
    acct       = join $ toAccount <$> (Right "8675309") <*> (Right "Ian Coleman") <*> (eitherDay userDay) <*> (toAmount 300 99) 
    
  print acct
  

