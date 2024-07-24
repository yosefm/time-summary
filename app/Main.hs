module Main (main) where

import Data.Maybe
import Data.Time

import Lib

main :: IO ()
main = do
    fileContent <- readFile "/mnt/c/Users/ymeller/presence.txt"
    mapM_ print $ filter (\wd -> getMonth wd == 7) $ mapMaybe parseWorkDay $ lines fileContent
  where getMonth wd = case (toGregorian . localDay . theDate) wd of
            (_, m, _) -> m
