import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                    then Right code
                                    else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"!@#"))
    ,(101,(Free,"SGJ"))
    ,(110,(Taken,"9929"))
    ]

-- Recursive data structures

-- Home baked list

-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

-- Home baked binary search tree

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a  = Node a (treeInsert x left) right
  | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a  = treeElem x left
  | x > a  = treeElem x right

-- usage:
-- let nums = [8,6,4,1,7,3,5]
-- let numsTree = foldr treeInsert EmptyTree nums
-- numsTree

class Eq' a where
  (.==) :: a -> a -> Bool
  (./=) :: a -> a -> Bool
  x .== y = not (x ./= y)
  x ./= y = not (x .== y)

data TrafficLight = Red | Yellow | Green

instance Eq' TrafficLight where
  Red .== Red = True
  Green .== Green = True
  Yellow .== Yellow = True
  _ .== _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

-- Functor typeclass

-- standard impl

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
  fmap' = map

instance Functor' Maybe where
  fmap' f (Just x) = Just (f x)
  fmap' f Nothing = Nothing

instance Functor' Tree where
  fmap' f EmptyTree = EmptyTree
  fmap' f (Node x leftsub rightsub) = Node (f x) (fmap' f leftsub) (fmap' f rightsub)

