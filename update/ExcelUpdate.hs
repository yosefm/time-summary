{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Data.Maybe
import Data.Time
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import Data.Time.Clock.POSIX

import Codec.Xlsx
import Control.Lens
import Lib

validateFormat :: Worksheet -> Bool
validateFormat sheet = 
    let dateHeaderCell = sheet ^? ixCell (3, 1) . cellValue . _Just
        toText (CellText txt) = txt
        toText _ = ""
    in maybe False ((== "Date ") . toText) dateHeaderCell
    
putName :: Worksheet -> T.Text -> Worksheet
putName sheet name = sheet & cellValueAt (2, 3) ?~ CellText name

main :: IO ()
main = do
    fileContent <- readFile "/mnt/c/Users/ymeller/presence.txt"
    srcXlsFile <- L.readFile "/mnt/c/Users/ymeller/Documents/Administrata/Monthly attendance report October 2024.xlsx"
    ct <- getPOSIXTime    
    
    let getMonth wd = case (toGregorian . localDay . theDate) wd of
            (_, m, _) -> m
        monthWorkedDays = filter (( == 10) . getMonth) $ mapMaybe parseWorkDay $ lines fileContent
        srcXlsx = toXlsx srcXlsFile 
        sheet = srcXlsx ^? ixSheet "Sheet1"
        updatedXlsx newSheet = srcXlsx & atSheet "Sheet1" ?~ newSheet
    
    case sheet of 
        Nothing -> putStrLn "Invalid template file: missing worksheet"
        Just s -> 
            case validateFormat s of 
                False -> putStrLn "Invalid template file: required date header not found"
                True -> L.writeFile "output.xlsx" (fromXlsx ct $ updatedXlsx $ putName s "Yosef Meller")
        
    
