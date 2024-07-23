module Main (main) where

import Control.Monad
import Data.Char (isSpace, toLower)
import Data.Maybe
import Data.List

import Data.Time
import Data.Time.Format

import Lib

type Clock = LocalTime
data DayContent = Vacation | SickLeave | Unknown
     | Worked Clock Clock
  deriving Show

data WorkDay = WorkDay {
    theDate :: LocalTime,
    content :: DayContent
  }
  deriving Show

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseWorkRange :: String -> Maybe DayContent
parseWorkRange s = 
    let (entryTimeStr, exitTimeStr) = break isSpace (dropWhile isSpace s)
        parseIt = parseTimeM True defaultTimeLocale "%k:%M"
        entryTime = parseIt entryTimeStr
        exitTime = parseIt $ takeWhile (not . isSpace) $ trim exitTimeStr
    in 
        liftA2 Worked entryTime exitTime

parseDayContent :: String -> DayContent
parseDayContent s
    | lower s == "vacation"  = Vacation
    | lower s == "sick day"  = SickLeave
    | otherwise = maybe Unknown id (parseWorkRange s)
    
  where lower = map toLower
  
parseWorkDay :: String -> Maybe WorkDay
parseWorkDay s =
    let (dateStr, rest) = break isSpace s
        parsedDate = parseTimeM True defaultTimeLocale "%e.%-m" dateStr
        retDay d = WorkDay d $ parseDayContent rest
    in 
        retDay <$> parsedDate

main :: IO ()
main = do
    fileContent <- readFile "/mnt/c/Users/ymeller/presence.txt"
    mapM_ print $ filter (\wd -> getMonth wd == 7) $ mapMaybe parseWorkDay $ lines fileContent
  where getMonth wd = case (toGregorian . localDay . theDate) wd of
            (_, m, _) -> m
