-- 1: Find the last element of a list.
f1 = myLast

myLast :: [a] -> a
myLast = last

--------------------------------------------------

-- 2: Find the last but one element of a list.
f2 = myButLast

myButLast :: [a] -> a
myButLast = last . init

--------------------------------------------------

-- 3: Find the K'th element of a list.
--    The first element in the list is number 1
f3 = elementAt

elementAt :: [a] -> Int -> a
elementAt = (!!)