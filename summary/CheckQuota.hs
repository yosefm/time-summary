module Main (main) where

import Data.Maybe
import Data.Time

import Options.Applicative
import Text.Read (readMaybe)

import Lib

data ProgramArgs = ProgramArgs {
    dataFile :: String
  , monthArg :: Int
  }

parseArgs :: Parser ProgramArgs
parseArgs = ProgramArgs 
  <$> strOption (
        value "/mnt/c/Users/ymeller/presence.txt"
    <>  long "datafile"
    <>  short 'd'
    <>  help "Path to file containing hours-worked table"
    )
  <*> argument auto (metavar "MONTH" <> help "The month to summarize")

progInfo :: ParserInfo ProgramArgs
progInfo = info (parseArgs <**> helper) (
    progDesc "Summarize worked time for a month from a text table"
  )
    
main :: IO ()
main = do
    args <- execParser progInfo
    fileContent <- readFile $ dataFile args
    
    let getMonth wd = case (toGregorian . localDay . theDate) wd of
            (_, m, _) -> m
        monthWorkedDays = filter (( == monthArg args) . getMonth) $ mapMaybe parseWorkDay $ lines fileContent
        
    mapM_ print $ monthWorkedDays
    putStrLn $ "Worked hours: " ++ formatFloatHours (workedHours monthWorkedDays)
    putStrLn $ "Required hours: " ++ formatFloatHours (requiredHours monthWorkedDays)
