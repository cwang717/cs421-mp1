--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake n [] = []
mytake n (x:xs)
  | n <= 0 = []
  | otherwise = x : mytake (n-1) xs

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop n [] = []
mydrop n (x:xs)
  | n <= 0 = (x:xs)
  | otherwise = mydrop (n-1) xs 

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev = revHelper []
  where
    revHelper newL [] = newL
    revHelper newL (x:xs) = revHelper (x:newL) xs

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app l1 l2 = appHelper l2 (rev l1)
  where
    appHelper l2 [] = l2
    appHelper l2 (x:xs) = appHelper (x:l2) xs

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x+1) : inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a, b)]
myzip [] l2 = []
myzip l1 [] = []
myzip (x:xs) (y:ys) = (x, y) : myzip xs ys

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: Num a => [a] -> [a] -> [a]
addpairs l1 l2 = addpairsHelper(myzip l1 l2)
  where
    addpairsHelper [] = []
    addpairsHelper ((a, b):abs) = (a+b) : addpairsHelper abs

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1:ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = [0..]

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : 1 : addpairs fib (tail fib)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add b oldL = addHelper [] b (rev oldL)
  where
    addHelper newL b [] = b:newL
    addHelper newL b (x:xs)
      | x > b = addHelper (x:newL) b xs
      | x == b = app (rev xs) (b:newL) 
      | otherwise = app (rev xs) (x:b:newL)

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union l1 l2 = unionHelper [] (rev l1) (rev l2)
  where
    unionHelper retL [] l2 = app (rev l2) retL
    unionHelper retL l1 [] = app (rev l1) retL
    unionHelper retL (x:xs) (y:ys)
      | x == y = unionHelper (x:retL) xs ys
      | x < y  = unionHelper (y:retL) (x:xs) ys
      | x > y  = unionHelper (x:retL) xs (y:ys)

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect l1 l2 = intersectHelper [] (rev l1) (rev l2)
  where
    intersectHelper retL [] l2 = retL
    intersectHelper retL l1 [] = retL
    intersectHelper retL (x:xs) (y:ys)
      | x == y = intersectHelper (x:retL) xs ys
      | x > y  = intersectHelper retL xs (y:ys)
      | x < y  = intersectHelper retL (x:xs) ys

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = let ps = powerset xs
                      withX = addXToAll x ps
                  in union ps withX
  where
    addXToAll x [] = []
    addXToAll x (ys:yss) = add x ys : addXToAll x yss

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' = P.map (+1)

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: Num a => [a] -> a
sumlist' = P.foldr (+) 0
