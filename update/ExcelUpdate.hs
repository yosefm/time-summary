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

-- File data format:
dateCol :: ColumnIndex
dateCol = 1

dateHeaderRow :: RowIndex
dateHeaderRow = 3

datePos, namePos :: (RowIndex, ColumnIndex)
datePos = (dateHeaderRow, dateCol)
namePos = (2, 3)

validateFormat :: Worksheet -> Bool
validateFormat sheet = 
    let dateHeaderCell = sheet ^? ixCell datePos . cellValue . _Just
        toText (CellText txt) = txt
        toText _ = ""
    in maybe False ((== "Date ") . toText) dateHeaderCell
    
putName :: Worksheet -> T.Text -> Worksheet
putName sheet name = sheet & cellValueAt namePos ?~ CellText name

putWorkDay :: Worksheet -> WorkDay -> Worksheet
putWorkDay sheet day = sheet & cellValueAt dayPos ?~ CellText "found it"
    where (_, _, d) = (toGregorian . localDay . theDate) day
          dayPos = (fromIntegral d + fst datePos, dateCol)

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
                True -> L.writeFile "output.xlsx" (
                    fromXlsx ct $ updatedXlsx 
                    $ flip putWorkDay (head monthWorkedDays)
                    $ putName s "Yosef Meller"
                  )
