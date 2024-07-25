module Main (main) where

import Data.Maybe
import Data.Time

import Lib

main :: IO ()
main = do
    fileContent <- readFile "/mnt/c/Users/ymeller/presence.txt"
    
    let getMonth wd = case (toGregorian . localDay . theDate) wd of
            (_, m, _) -> m
        monthWorkedDays = filter (\wd -> getMonth wd == 7) $ mapMaybe parseWorkDay $ lines fileContent
        
    mapM_ print $ monthWorkedDays
    putStrLn $ "Worked hours: " ++ show (workedHours monthWorkedDays)
    putStrLn $ "Required hours: " ++ show (requiredHours monthWorkedDays)
