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

