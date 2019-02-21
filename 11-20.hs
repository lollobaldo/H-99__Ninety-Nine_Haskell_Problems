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
-- **15: Replicate the elements of a list a given number of times.
f15 = repli

repli :: [a] -> Int -> [a]
repli ls n = concat . map (replicate n) $ ls

prop_f15 = f15 "abc" 3 == "aaabbbccc"

--------------------------------------------------
-- **16: Drop every N'th element from a list.
f16 = dropEvery

dropEvery :: [a] -> Int -> [a]
dropEvery ls n = [l | (l,i) <- zip ls [1..], i `mod` n /= 0]

prop_f16 = f16 "abcdefghik" 3 == "abdeghk"

--------------------------------------------------
-- *17: Split a list into two parts; the length of the first part is given.
f17 = split

split :: [a] -> Int -> ([a],[a])
split = flip splitAt

prop_f17 = f17 "abcdefghik" 3 == ("abc","defghik")

--------------------------------------------------
-- **18: Given two indeces, i and k, the slice is the list containing the
-- **    elements between the i'th and k'th element of the original list (both
-- **    limist included). Start counting the element with 1.
f18 = slice

slice :: [a] -> Int -> Int -> [a]
slice ls a b = drop (a-1) $ take b ls

prop_f18 = f18 "abcdefghik" 3 7 == "cdefg"

--------------------------------------------------
-- **19: Rotate a list N placed to the left.
f19 = rotate

rotate :: [a] -> Int -> [a]
rotate = group

prop_f19 = f19 ['a', 'b', 'c', 'd', 'e', 'f', 'g', h'] 3    == "defghabc"
        && f19 ['a', 'b', 'c', 'd', 'e', 'f', 'g', h'] (-2) == "ghabcdef"

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
