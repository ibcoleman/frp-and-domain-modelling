module Date () where

import Data.Time
import Control.Monad(join)
import Text.Read(readMaybe)

getDayFromUTCTime :: UTCTime -> Day
getDayFromUTCTime (UTCTime day time) = day

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


