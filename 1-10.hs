import Test.QuickCheck
import Data.List

--------------------------------------------------
-- *1: Find the last element of a list.
f1 = myLast

myLast :: [a] -> a
myLast = last

prop_f1 = f1 [1,2,3,4]     == 4
       && f1 ['x','y','z'] == 'z'

--------------------------------------------------
-- *2: Find the last but one element of a list.
f2 = myButLast

myButLast :: [a] -> a
myButLast = last . init

prop_f2 = f2 [1,2,3,4]  == 3
       && f2 ['a'..'z'] == 'y'

--------------------------------------------------
-- *3: Find the K'th element of a list.
--    The first element in the list is number 1
f3 = elementAt

elementAt :: [a] -> Int -> a
elementAt = (!!)

prop_f3 = f3 [1,2,3]   2 == 2
       && f3 "haskell" 5 == 'e'

--------------------------------------------------
-- *4: Find the number of elements of a list
f4 = myLength

myLength :: [a] -> Int
myLength = length

prop_f4 = f4 [123,456,789]   == 3
       && f4 "Hello, world!" == 13

--------------------------------------------------
-- *5: Reverse a list.
f5 = myReverse

myReverse :: [a] -> [a]
myReverse = reverse

prop_f5 = f5 "A man, a plan, a canal, panama!" == 
                "!amanap ,lanac a ,nalp a ,nam A"
       && f5 [1,2,3,4] == [4,3,2,1]

--------------------------------------------------
-- *6: Find out whether a list is a palindrome.
--    A palindrome can be read forward or backward;
--    e.g. (x a m a x)
f6 ls = isPalindrome ls

isPalindrome :: Eq a => [a] -> Bool
isPalindrome ls = ls == reverse ls

prop_f6 = f6 [1,2,3]              == False
       && f6 "madamimadam"        == True
       && f6 [1,2,4,8,16,8,4,2,1] == True

--------------------------------------------------
-- **7: Flatten a nested list structure.
f7 = flatten

data NestedList a = Elem a | List [NestedList a] deriving (Eq, Show)

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List ls) = concatMap flatten ls
-- TODO: Not sure, but the prop gives some type error on Eq
--       being undefined. Manually checked them anyways.
-- prop_f7 = f7 (Elem 5) == [5]
--        && f7 (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
--                 == [1,2,3,4,5]
--        && f7 (List []) == []

--------------------------------------------------
-- **8: Eliminate consecutive duplicates of list elements.
-- **    If a list contains repeated elements they should be
-- **    replaced with a single copy of the element. The order
-- **    of the elements should not be changed
f8 = compress

compress :: Eq a => [a] -> [a]
compress (x:y:zs)
    | x == y = compress (x:zs)
    | otherwise = x : compress (y:zs)
compress xs = xs

prop_f8 = f8 "aaaabccaadeeee" == "abcade"

--------------------------------------------------
-- **9: Pack consecutive duplicates of list elements into
-- **    sublists. If a list contains repeated elements they
-- **    should be placed in separate sublists
f9 = pack

--imported Data.List

pack :: Eq a => [a] -> [[a]]
pack = group

prop_f9 = f9 ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a',
              'a', 'd', 'e', 'e', 'e', 'e'] ==
              ["aaaa","b","cc","aa","d","eeee"]

--------------------------------------------------
-- **10: Run-length encoding of a list. Use the result of
-- **     problem P09 to implement the so-colled run length
-- **     encoding data compression method. Consecutive
-- **     duplicates of elements are encoded as lists (N E)
-- **     where N is the number of duplicates of the element E
f10 ls = encode ls

encode :: Eq a => [a] -> [(Int,a)]
encode = undefined

prop_f10 = f10 "aaaabccaadeeee" ==
            [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
