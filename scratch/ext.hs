module Scratch.Ext where
import Advent.Util.ListUtil 

x1 = [["1", "2"]]
y1 = [["3", "4"], ["5", "6"]]
a1 =  ext <$> x1
-- print a1 <*> y1


extList:: [[[a]]]

let a = [["1", "2"]]
let b = y1 = [["3", "4"], ["5", "6"]]

let f = (flip ext) (head b)
fmap (\m -> f m) a
