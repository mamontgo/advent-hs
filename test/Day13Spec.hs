module Day13Spec(runDay13) where
import Advent.Day13.Mirror (score, getPoints, scoreAll, Points)
import Test.Framework

runDay13:: IO () 
runDay13 = do 
        ex1
        ex7
        ex9
        day13Part1PrintHeadResult
        day13Part1PrintAllResult
        day13Part1PrintFinalResult



ex1:: IO ()
ex1 = do 
        headPointsPrint "./data/day13/ex1.txt" "EX1 head score " 
        assertIO (getScore "./data/day13/ex1.txt") 2 

ex7:: IO ()
ex7 = do 
        headPointsPrint "./data/day13/ex7.txt" "EX7 head score " 
        assertIO (getScore "./data/day13/ex7.txt") 16 



ex9:: IO ()
ex9 = do 
        headPointsPrint "./data/day13/ex9.txt" "EX9 head score " 
        assertIO (getScore "./data/day13/ex9.txt") 100 

day13Part1PrintHeadResult:: IO ()
day13Part1PrintHeadResult = do 
        headPointsPrint "./data/day13/test.txt" "Head score "
        assertIO (getScore "./data/day13/test.txt") 5 




day13Part1PrintAllResult:: IO ()
day13Part1PrintAllResult = 
                        do
                                s <- (getTotalScore "./data/day13/test.txt")
                                print "Total score" 
                                print s
                                assertIO (getTotalScore "./data/day13/test.txt") 405

day13Part1PrintFinalResult:: IO ()
day13Part1PrintFinalResult = 
                        do
                                res <- scoreAll <$> points
                                print "Result score" 
                                print res
                                assertIO (getTotalScore "./data/day13/res.txt") 27742
                        where
                                points = getPoints "./data/day13/res.txt"

assertIO:: IO Int -> Int -> IO ()
assertIO i v = do
                e <- i
                assertEqual e v

getTotalScore:: String -> IO Int
getTotalScore f = scoreAll <$> getPoints f    

getScore:: String -> IO Int
getScore f = score <$> headPoints f    

headPointsPrint:: String -> String -> IO ()
headPointsPrint f m = 
                        do
                                res <- score <$> points
                                print m
                                print res
                        where
                                points = headPoints f

headPoints:: String -> IO [Points]
headPoints f = head <$> getPoints f