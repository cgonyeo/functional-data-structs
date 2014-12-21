data SplayHeap a = SEmpty
                 | SNode (SplayHeap a) a (SplayHeap a) deriving(Show,Eq)

insert :: (Eq a,Show a,Ord a) => a -> SplayHeap a -> SplayHeap a
insert x t = SNode (smaller x t) x (bigger x t)

bigger :: (Eq a,Show a,Ord a) => a -> SplayHeap a -> SplayHeap a
bigger pivot SEmpty = SEmpty
bigger pivot (SNode a x b) =
        if x <= pivot
            then bigger pivot b
            else case a of
                     SEmpty -> SNode SEmpty x b
                     SNode ll y lr -> if y <= pivot
                                         then SNode (bigger pivot lr) x b
                                         else SNode (bigger pivot ll) y (SNode lr x b)

smaller :: (Eq a,Show a,Ord a) => a -> SplayHeap a -> SplayHeap a
smaller pivot SEmpty = SEmpty
smaller pivot (SNode a x b) =
        if x > pivot
            then smaller pivot a
            else case b of
                     SEmpty -> SNode a x SEmpty
                     SNode rl y rr -> if y > pivot
                                         then SNode a x (smaller pivot rl)
                                         else SNode (SNode a x rl) y (smaller pivot rr)

findMin :: (Eq a,Show a,Ord a) => SplayHeap a -> Maybe a
findMin SEmpty = Nothing
findMin (SNode SEmpty x b) = Just x
findMin (SNode a x b) = findMin a

deleteMin :: (Eq a,Show a,Ord a) => SplayHeap a -> SplayHeap a
deleteMin (SNode SEmpty x b) = b
deleteMin (SNode (SNode SEmpty x b) y c) = SNode b y c
deleteMin (SNode (SNode a x b) y c) = SNode (deleteMin a) x (SNode b y c)

fromList :: (Eq a,Show a,Ord a) => [a] -> SplayHeap a
fromList = foldr insert SEmpty

toList :: (Eq a,Show a,Ord a) => SplayHeap a -> [a]
toList t = case findMin t of
               Nothing -> []
               Just x -> x:(toList $ deleteMin t)

main :: IO ()
main = do putStrLn $ "Sorting: " ++ show [0..10]
          let t1 = fromList [0..10]
          print $ toList t1
          putStrLn $ "Sorting: " ++ show [10,9..0]
          let t2 = fromList [10,9..0]
          print $ toList t2
          putStrLn $ "Sorting: " ++ show [1,3,5,7,9,2,4,6,8,10,0]
          let t3 = fromList [1,3,5,7,9,2,4,6,8,10,0]
          print $ toList t3
