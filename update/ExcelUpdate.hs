{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import System.FilePath

import Data.Maybe
import Data.Time
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import Data.Time.Clock.POSIX

import Codec.Xlsx
import Control.Lens
import Lib

import qualified Options.Applicative as OA
import Options.Applicative (execParser, 
    strArgument, metavar, help, value, 
    strOption, short, long,
    helper, info, progDesc, (<**>))
import Args (baseArgs)

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

-- Substitutions:
putName :: Worksheet -> T.Text -> Worksheet
putName sheet name = sheet & cellValueAt namePos ?~ CellText name

putWorkDay :: WorkDay -> Worksheet -> Worksheet
putWorkDay day sheet = 
    case content day of 
        (Worked entry exit) -> clockedUpdate entry exit
        (HalfWorked entry exit) -> clockedUpdate entry exit
        Vacation -> emptyUpdate & cellValueAt notePos ?~ CellText "Vacation"
        SickLeave -> emptyUpdate & cellValueAt notePos ?~ CellText "Sick leave"
        _ -> emptyUpdate
    
    where (_, _, d) = (toGregorian . localDay . theDate) day
          entryPos = (fromIntegral d + fst datePos, dateCol + 1)
          exitPos = (fromIntegral d + fst datePos, dateCol + 2)
          notePos = (fromIntegral d + fst datePos, dateCol + 3)
          ft = T.pack . formatTime defaultTimeLocale "%H:%M" 
          
          clockedUpdate entry' exit' = 
            sheet & cellValueAt entryPos ?~ CellText (ft entry')
                  & cellValueAt exitPos ?~ CellText (ft exit')
          
          emptyUpdate = 
            sheet & cellValueAt entryPos .~ Nothing
                  & cellValueAt exitPos .~ Nothing

updateSheet :: Worksheet -> [WorkDay] -> T.Text -> Worksheet
updateSheet sheet workedDays employeeName = 
    let updateDays s = foldr putWorkDay s workedDays
    in updateDays $ putName sheet employeeName

-- CL arguments:
data ProgramArgs = ProgramArgs {
    dataFile :: String
  , monthArg :: Int
  , excelSrcPath :: String
  , employeeName :: String
  }


parseArgs :: OA.Parser ProgramArgs
parseArgs = baseArgs ProgramArgs 
    <*> strArgument (
           metavar "SRC" 
        <> help "Liron's incoming file"
        <> value "/mnt/c/Users/ymeller/Documents/Administrata/Monthly attendance report October 2024.xlsx"
        )
    <*> strOption (
        value "Yosef Meller"
    <>  long "employee-name"
    <>  short 'e'
    <>  help "Name to use in output path and inside file."
        )

progInfo :: OA.ParserInfo ProgramArgs
progInfo = info (parseArgs <**> helper) (
    progDesc "Fill in presence from data file into excel"
  )

main :: IO ()
main = do
    args <- execParser progInfo
    fileContent <- readFile $ dataFile args
    srcXlsFile <- L.readFile $ excelSrcPath args
    ct <- getPOSIXTime    
    
    let getMonth wd = case (toGregorian . localDay . theDate) wd of
            (_, m, _) -> m
        monthWorkedDays = filter (( == monthArg args) . getMonth) $ mapMaybe parseWorkDay $ lines fileContent
        
        srcXlsx = toXlsx srcXlsFile 
        sheet = srcXlsx ^? ixSheet "Sheet1"
        updatedXlsx newSheet = srcXlsx & atSheet "Sheet1" ?~ newSheet
        
        (fname, fext) = splitExtension (excelSrcPath args)
        outputPath = concat [takeBaseName fname, " ", employeeName args] `addExtension` fext 
    
    case sheet of 
        Nothing -> putStrLn "Invalid template file: missing worksheet"
        Just s -> 
            case validateFormat s of 
                False -> putStrLn "Invalid template file: required date header not found"
                True -> L.writeFile outputPath (
                    fromXlsx ct $ updatedXlsx 
                    $ updateSheet s monthWorkedDays $ T.pack $ employeeName args
                  )
