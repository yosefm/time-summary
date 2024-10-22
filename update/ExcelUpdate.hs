module Main (main) where

import Data.Maybe
import Data.Time

import Codec.Xlsx
import Lib

main :: IO ()
main = do
    fileContent <- readFile "/mnt/c/Users/ymeller/presence.txt"
    
    let getMonth wd = case (toGregorian . localDay . theDate) wd of
            (_, m, _) -> m
        monthWorkedDays = filter (( == 10) . getMonth) $ mapMaybe parseWorkDay $ lines fileContent
        -- updateSheet = 
    
    return ()
        
