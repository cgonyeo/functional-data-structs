data Tree a = Empty | Node (Tree a) a (Tree a) deriving(Show,Eq)

insert :: (Eq a,Show a,Ord a) => a -> Tree a -> Tree a
insert x t = Node (smaller x t) x (bigger x t)

bigger :: (Eq a,Show a,Ord a) => a -> Tree a -> Tree a
bigger pivot Empty = Empty
bigger pivot (Node a x b) =
        if x <= pivot
            then bigger pivot b
            else case a of
                     Empty -> Node Empty x b
                     Node ll y lr -> if y <= pivot
                                         then Node (bigger pivot lr) x b
                                         else Node (bigger pivot ll) y (Node lr x b)

smaller :: (Eq a,Show a,Ord a) => a -> Tree a -> Tree a
smaller pivot Empty = Empty
smaller pivot (Node a x b) =
        if x > pivot
            then smaller pivot a
            else case b of
                     Empty -> Node a x Empty
                     Node rl y rr -> if y > pivot
                                         then Node a x (smaller pivot rl)
                                         else Node (Node a x rl) y (smaller pivot rr)

findMin :: (Eq a,Show a,Ord a) => Tree a -> a
findMin (Node Empty x b) = x
findMin (Node a x b) = findMin a

deleteMin :: (Eq a,Show a,Ord a) => Tree a -> Tree a
deleteMin (Node Empty x b) = b
deleteMin (Node (Node Empty x b) y c) = Node b y c
deleteMin (Node (Node a x b) y c) = Node (deleteMin a) x (Node b y c)

fromList :: (Eq a,Show a,Ord a) => [a] -> Tree a
fromList = foldr insert Empty

toList :: (Eq a,Show a,Ord a) => Tree a -> [a]
toList Empty = []
toList t = (findMin t):(toList $ deleteMin t)

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
          
