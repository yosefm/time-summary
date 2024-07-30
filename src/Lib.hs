module Lib ( 
    WorkDay(..), DayContent, parseWorkDay, 
    workedHours, requiredHours 
  ) where

import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import Data.Time

type Clock = LocalTime
data DayContent = Vacation | SickLeave | Unknown
     | Worked Clock Clock
  deriving Show

data WorkDay = WorkDay {
    theDate :: LocalTime,
    content :: DayContent
  }
  deriving Show

isWorked :: DayContent -> Bool
isWorked (Worked _ _) = True
isWorked _ = False

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
  where addWorked (Worked entry exit) dt = dt + (realToFrac $ diffLocalTime exit entry)
        addWorked _ dt = dt

requiredHours :: [WorkDay] -> Float
requiredHours = (/5) . fromIntegral . (*42) . length . filter (isWorked . content)
