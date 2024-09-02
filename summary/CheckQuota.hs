module Main (main) where

import Data.Maybe
import Data.Time

import System.Environment
import System.Exit
import Text.Read (readMaybe)

import Lib

parseMonthArg :: [String] -> IO Int
parseMonthArg [] = exitFailure
parseMonthArg [month] = 
    case readMaybe month of 
        Nothing -> exitFailure
        Just m -> return m
parseMonthArg _ = exitFailure

main :: IO ()
main = do
    fileContent <- readFile "/mnt/c/Users/ymeller/presence.txt"
    selectMonth <- getArgs >>= parseMonthArg
    
    let getMonth wd = case (toGregorian . localDay . theDate) wd of
            (_, m, _) -> m
        monthWorkedDays = filter (( == selectMonth) . getMonth) $ mapMaybe parseWorkDay $ lines fileContent
        
    mapM_ print $ monthWorkedDays
    putStrLn $ "Worked hours: " ++ show (workedHours monthWorkedDays)
    putStrLn $ "Required hours: " ++ show (requiredHours monthWorkedDays)
