module Args (baseArgs) where

import Options.Applicative

baseArgs :: Read a => (String -> a -> c) -> Parser c
baseArgs f = f <$> strOption (
        value "/mnt/c/Users/ymeller/presence.txt"
    <>  long "datafile"
    <>  short 'd'
    <>  help "Path to file containing hours-worked table"
    )
  <*> argument auto (metavar "MONTH" <> help "The month to summarize")
