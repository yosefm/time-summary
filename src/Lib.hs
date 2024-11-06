module Lib ( 
    WorkDay(..), DayContent(..), parseWorkDay, 
    workedHours, requiredHours 
  ) where

import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import Data.Bool (bool)
import Data.Time

type Clock = LocalTime
data DayContent = Vacation | SickLeave | Unknown
     | Worked Clock Clock
     | HalfWorked Clock Clock
  deriving Show

data WorkDay = WorkDay {
    theDate :: LocalTime,
    content :: DayContent
  }
  deriving Show

-- how many days' work is expected from one day?
requiredWork :: DayContent -> Float
requiredWork (Worked _ _) = 1
requiredWork (HalfWorked _ _) = 0.5
requiredWork _ = 0

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseWorkRange :: String -> Maybe DayContent
parseWorkRange s = 
    let (entryTimeStr, rest) = break isSpace (dropWhile isSpace s)
        parseIt = parseTimeM True defaultTimeLocale "%k:%M"
        entryTime = parseIt entryTimeStr
        (exitTimeStr, note) = break isSpace (dropWhile isSpace rest)
        exitTime = parseIt $ takeWhile (not . isSpace) $ trim exitTimeStr
        construct = bool Worked HalfWorked $ take 4 (trim note) == "half"
    in 
        liftA2 construct entryTime exitTime

parseDayContent :: String -> DayContent
parseDayContent s
    | lower s == "vacation"  = Vacation
    | lower s == "sick day"  = SickLeave
    | otherwise = fromMaybe Unknown (parseWorkRange s)
    
  where lower = map toLower
  
parseWorkDay :: String -> Maybe WorkDay
parseWorkDay s =
    let (dateStr, rest) = break isSpace s
        parsedDate = parseTimeM True defaultTimeLocale "%e.%-m" dateStr
        retDay d = WorkDay d $ parseDayContent $ trim rest
    in 
        retDay <$> parsedDate

workedHours :: [WorkDay] -> Float
workedHours = (/3600) . foldr (addWorked . content) 0 
  where addWorked (Worked entry exit) dt = dt + rangeVal entry exit
        addWorked (HalfWorked entry exit) dt = dt + rangeVal entry exit
        addWorked _ dt = dt
        rangeVal entry exit = (realToFrac $ diffLocalTime exit entry)

requiredHours :: [WorkDay] -> Float
requiredHours = (* (42/5)) . sum . map (requiredWork . content) 
