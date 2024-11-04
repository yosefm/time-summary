{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Data.Maybe
import Data.Time
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L

import Codec.Xlsx
import Control.Lens
import Lib

validateFormat :: Worksheet -> Bool
validateFormat sheet = 
    let dateHeaderCell = sheet ^? ixCell (3, 1) . cellValue . _Just
        toText (CellText txt) = txt
        toText _ = ""
    in maybe False ((== "Date ") . toText) dateHeaderCell

main :: IO ()
main = do
    fileContent <- readFile "/mnt/c/Users/ymeller/presence.txt"
    srcXlsFile <- L.readFile "/mnt/c/Users/ymeller/Documents/Administrata/Monthly attendance report October 2024.xlsx"
    
    let getMonth wd = case (toGregorian . localDay . theDate) wd of
            (_, m, _) -> m
        monthWorkedDays = filter (( == 10) . getMonth) $ mapMaybe parseWorkDay $ lines fileContent
        sheet = toXlsx srcXlsFile ^? ixSheet "Sheet1"
    
    case validateFormat <$> sheet of 
        Nothing -> putStrLn "Invalid template file: meesing worksheet"
        Just False -> putStrLn "Invalid template file: required date not found"
        Just True -> putStrLn "Awesome template file"
        
    
