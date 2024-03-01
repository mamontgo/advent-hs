module Day16Spec (runDay16) where

import Advent.Day16.Path (points, pointsTraveled, test, readPt)
import Test.Framework (assertEqual)

runDay16 :: IO ()
runDay16 = do
  part1TestTravelledPoints
  part1ResTravelledPoints


part1TestTravelledPoints:: IO ()
part1TestTravelledPoints = do
                        let score = pointsTraveled $ points test
                        print ("Part2 Test Score " ++ show score) 
                        assertEqual score 46


part1ResTravelledPoints:: IO ()
part1ResTravelledPoints = do
                        pts <- readPt "./data/day16/res.txt"
                        let score = pointsTraveled $ points pts
                        print ("Part2 Res Score " ++ show score) 
                        assertEqual score 8021
