{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Maybe
import Data.Time

import Options.Applicative
import Text.Read (readMaybe)
import Text.Printf (printf)

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

formatFloatHours :: Float -> String
formatFloatHours t = 
    let h :: Int = truncate t
        m :: Int = round $ (t - fromIntegral h) * 60
    in printf "%02d:%02d" h m 
    
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
