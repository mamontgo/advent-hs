module Advent.Util.FileUtil(load) where

load:: String -> IO [String]
load f =  lines <$> readFile f 

