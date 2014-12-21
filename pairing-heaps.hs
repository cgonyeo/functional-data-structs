data PairingHeap a = PEmpty | PNode a [PairingHeap a]

findMin :: (Show a,Ord a,Eq a) => PairingHeap a -> Maybe a
findMin PEmpty = Nothing
findMin (PNode a _) = Just a

merge :: (Show a,Ord a,Eq a) => PairingHeap a -> PairingHeap a -> PairingHeap a
merge PEmpty PEmpty = PEmpty
merge h PEmpty = h
merge PEmpty h = h
merge (PNode x xs) (PNode y ys) = 
        if x <= y
            then PNode x (PNode y ys:xs)
            else PNode y (PNode x xs:ys)

insert :: (Show a,Ord a,Eq a) => a -> PairingHeap a -> PairingHeap a
insert x h = merge (PNode x []) h

mergePairs :: (Show a,Ord a,Eq a) => [PairingHeap a] -> PairingHeap a
mergePairs [] = PEmpty
mergePairs [h] = h
mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)

deleteMin :: (Show a,Ord a,Eq a) => PairingHeap a -> PairingHeap a
deleteMin PEmpty = PEmpty
deleteMin (PNode x hs) = mergePairs hs

fromList :: (Show a,Ord a,Eq a) => [a] -> PairingHeap a
fromList = foldr insert PEmpty

toList :: (Show a,Ord a,Eq a) => PairingHeap a -> [a]
toList PEmpty = []
toList ph = case findMin ph of
                Nothing -> toList $ deleteMin ph
                Just x -> x:(toList $ deleteMin ph)

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
