import Day13Spec (runDay13)
import Day14Spec (runDay14)
import ListUtilSpec (runListUtil)

main :: IO ()
main = do

                printTestRes
                runListUtil
                runDay13
                runDay14 




printTestRes:: IO() 
printTestRes = do
                    print "Welcome To Tests"