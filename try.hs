import Data.List

x :: [Int] -> [Int]
x = foldr (\x y -> y ++ [2*x]) []

lister :: [Int] -> [[Int]]
lister ls = [switch i l | (i,l) <- zip [0..9] (repeat ls)]

switch :: Int -> [Int] -> [Int]
switch 0 ls = ls
switch n ls = [if i == n && x == -1 then 0 else x | (i,x) <- zip [1..] ls]

data ShoppingList a
  = Empty
  | Item a 
  | Listofitems [ShoppingList a]
  deriving (Show)

instance Functor ShoppingList where
  -- fmap :: (a -> b) -> ShoppingList a -> ShoppingList b
  fmap _ Empty = Empty
  fmap f (Item i) = Item (f i)
  fmap f (Listofitems ls) = Listofitems $ map (fmap f) ls


sumAll :: [Int] -> Int
sumAll ls = foldl' (+) 0 ls


sumAny a b = sumAny (a+b)
sumAny ret = ret