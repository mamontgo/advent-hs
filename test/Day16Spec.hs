module Day16Spec (runDay16) where

import Advent.Day16.Path (maxVisitPoints, points, pointsTraveled, readPt, test)
import Test.Framework (assertEqual)

runDay16 :: IO ()
runDay16 = do
  part1TestTravelledPoints
  part1ResTravelledPoints
  part2TestTravelledPoints

part1TestTravelledPoints :: IO ()
part1TestTravelledPoints = do
  let score = pointsTraveled $ points test
  print ("Part1 Test Score " ++ show score)
  assertEqual score 46

part1ResTravelledPoints :: IO ()
part1ResTravelledPoints = do
  pts <- readPt "./data/day16/res.txt"
  let score = pointsTraveled $ points pts
  print ("Part1 Res Score " ++ show score)
  assertEqual score 8021

part2TestTravelledPoints :: IO ()
part2TestTravelledPoints = do
  let score = maxVisitPoints $ points test
  print ("Part2 Test Score " ++ show score)
  assertEqual score 51