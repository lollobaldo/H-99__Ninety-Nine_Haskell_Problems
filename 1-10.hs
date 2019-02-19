import Test.QuickCheck

--------------------------------------------------
-- 1: Find the last element of a list.
f1 = myLast

myLast :: [a] -> a
myLast = last

prop_f1 = f1 [1,2,3,4]     == 4
       && f1 ['x','y','z'] == 'z'

--------------------------------------------------
-- 2: Find the last but one element of a list.
f2 = myButLast

myButLast :: [a] -> a
myButLast = last . init

prop_f2 = f2 [1,2,3,4]  == 3
       && f2 ['a'..'z'] == 'y'

--------------------------------------------------
-- 3: Find the K'th element of a list.
--    The first element in the list is number 1
f3 = elementAt

elementAt :: [a] -> Int -> a
elementAt = (!!)

prop_f3 = f3 [1,2,3]   2 == 2
       && f3 "haskell" 5 == 'e'