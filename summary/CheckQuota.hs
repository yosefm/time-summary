module Main (main) where

import Data.Maybe
import Options.Applicative

import Lib
import Args (baseArgs)

data ProgramArgs = ProgramArgs {
    dataFile :: String
  , monthArg :: Int
  }

parseArgs :: Parser ProgramArgs
parseArgs = baseArgs ProgramArgs 

progInfo :: ParserInfo ProgramArgs
progInfo = info (parseArgs <**> helper) (
    progDesc "Summarize worked time for a month from a text table"
  )
    
main :: IO ()
main = do
    args <- execParser progInfo
    fileContent <- readFile $ dataFile args
    
    let monthWorkedDays = filter (( == monthArg args) . getMonth) $ mapMaybe parseWorkDay $ lines fileContent
        
    mapM_ print $ monthWorkedDays
    putStrLn $ "Worked hours: " ++ formatFloatHours (workedHours monthWorkedDays)
    putStrLn $ "Required hours: " ++ formatFloatHours (requiredHours monthWorkedDays)
