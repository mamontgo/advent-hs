module Advent.Util.ListPoint (variations) where

data ListPoint a = ListPoint [a] (Maybe a) [a]

instance Foldable ListPoint where
  foldr _ z (ListPoint _ Nothing _) = z
  foldr f z l@(ListPoint _ (Just p) _) = p `f` foldr f z (next l)

instance Functor ListPoint where
    fmap f (ListPoint s Nothing e) = ListPoint (f <$> s) Nothing (f <$> e)
    fmap f (ListPoint s (Just p) e) = ListPoint (f <$> s) (Just $ f p) (f <$> e)

empty :: ListPoint a
empty = ListPoint [] Nothing []

listPoint :: [a] -> ListPoint a
listPoint [] = empty
listPoint (h : t) = ListPoint [] (Just h) t

next :: ListPoint a -> ListPoint a
next (ListPoint _ Nothing _) = empty
next (ListPoint s (Just a) []) = ListPoint (s ++ [a]) Nothing []
next (ListPoint s (Just a) (h : t)) = ListPoint (s ++ [a]) (Just h) t

asList :: ListPoint a -> [a]
asList (ListPoint s Nothing e) = s ++ e
asList (ListPoint s (Just p) e) = s ++ [p] ++ e

applyPoint:: (a -> a) -> ListPoint a -> ListPoint a
applyPoint _ l@(ListPoint _ Nothing _) = l
applyPoint f (ListPoint s (Just p) e) = ListPoint s (Just $ f p) e

variations:: (a -> a) -> [a] -> [[a]]
variations f p = listPoints f (listPoint p) 

listPoints:: (a -> a) -> ListPoint a -> [[a]]
listPoints _ (ListPoint _ Nothing _) = []
listPoints f p =  h : listPoints f (next p)
                    where h = asList $ applyPoint f p


