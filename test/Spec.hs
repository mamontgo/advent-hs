import Day13Spec (runDay13)
import ListUtilSpec (runListUtil)
main :: IO ()
main = do
                printTestRes
                runListUtil
                runDay13




printTestRes:: IO() 
printTestRes = do
                    print "Welcome To Tests"