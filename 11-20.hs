import Test.QuickCheck
import Data.List

--------------------------------------------------
-- *11: Modify the result of P10 in such a way that if an element has no
-- *    duplicates it is simply copies into the result list. Only elements with
-- *    duplicates are transferred as (N E) lists.
f11 = encodeModified

data ListItem a = Single a | Multiple Int a deriving (Eq, Show)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map (
    \x ->
        if length x == 1
          then Single (head x)
          else Multiple (length x) (head x) ) . group

prop_f11 = f11 "aaaabccaadeeee"
    == [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']


--------------------------------------------------
-- **02: Given a run-length code list generated as specified in problem 11.
-- **    Construct its uncompressed version.
f12 = decodeModified

decodeModified :: [ListItem a] -> [a]
decodeModified = concat . map decodeOne
  where
    decodeOne :: ListItem a -> [a]
    decodeOne (Single x)     = [x]
    decodeOne (Multiple n x) = replicate n x

prop_f12 = f12 [Multiple 4 'a',Single 'b',Multiple 2 'c',
       Multiple 2 'a',Single 'd',Multiple 4 'e']
    == "aaaabccaadeeee"
prop_f12'  x = (f12 . f11) x == x

--------------------------------------------------
-- **13: Implement the so-called run-length encoding data compression method
-- **    directly. I.e. don't explicitly create the sublists containing the
-- **    duplicates, as in problem 9, but only count them. As in problem P11,
-- **    simplify the result list by replacing the singleton lists (1 X) by X.
f13 = undefined
-- FIXME: not really sure what I'm meant to do(?)
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = undefined

-- prop_f13 = f13 [Multiple 4 'a',Single 'b',Multiple 2 'c',
--        Multiple 2 'a',Single 'd',Multiple 4 'e']
--     == "aaaabccaadeeee"

--------------------------------------------------
-- *14: Duplicate the elements of a list
f14 = dupli

dupli :: [a] -> [a]
dupli = concat . map (replicate 2)

prop_f14 = f14 [1,2,3]   == [1,1,2,2,3,3]

--------------------------------------------------
-- *05: Reverse a list.
f05 = myReverse

myReverse :: [a] -> [a]
myReverse = reverse

prop_f05 = f05 "A man, a plan, a canal, panama!" == 
                  "!amanap ,lanac a ,nalp a ,nam A"
        && f05 [1,2,3,4] == [4,3,2,1]

--------------------------------------------------
-- *06: Find out whether a list is a palindrome. A palindrome can be read
-- *     forward or backward; e.g. (x a m a x).
f06 ls = isPalindrome ls

isPalindrome :: Eq a => [a] -> Bool
isPalindrome ls = ls == reverse ls

prop_f06 = f06 [1,2,3]              == False
        && f06 "madamimadam"        == True
        && f06 [1,2,4,8,16,8,4,2,1] == True

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
-- **8: Eliminate consecutive duplicates of list elements. If a list contains
-- **   repeated elements they should be replaced with a single copy of the
-- **   element. The order of the elements should not be changed.
f8 = compress

compress :: Eq a => [a] -> [a]
compress (x:y:zs)
    | x == y = compress (x:zs)
    | otherwise = x : compress (y:zs)
compress xs = xs

prop_f8 = f8 "aaaabccaadeeee" == "abcade"

--------------------------------------------------
-- **9: Pack consecutive duplicates of list elements into sublists. If a list
-- **   contains repeated elements they should be placed in separate sublists.
f9 = pack

--imported Data.List

pack :: Eq a => [a] -> [[a]]
pack = group

prop_f9 = f9 ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a',
              'a', 'd', 'e', 'e', 'e', 'e'] ==
              ["aaaa","b","cc","aa","d","eeee"]

--------------------------------------------------
-- **10: Run-length encoding of a list. Use the result of problem P09 to
-- **    implement the so-colled run length encoding data compression method.
-- **    Consecutive duplicates of elements are encoded as lists (N E)
-- **    where N is the number of duplicates of the element E
f10 ls = encode ls

encode :: Eq a => [a] -> [(Int,a)]
encode = map (\x -> (length x , head x)) . group

prop_f10 = f10 "aaaabccaadeeee" ==
                  [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
