import Data.List

data BinTree a = Empty
                 | Node a (BinTree a) (BinTree a)
                            deriving (Eq,Ord)

instance Show (BinTree a) where
    show t =

treeFromList :: (Ord a) => [a] -> BinTree a
treeFromList [] = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
                             (treeFromList (filter (>x) xs))

main = print $ treeFromList [7,2,4,8]