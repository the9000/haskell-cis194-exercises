-- 03: code golf

import GHC.Exts
import Data.List (intersperse)

{- 1
The output of skips is a list of lists.
The first list in the output should
be the same as the input list. The second list in the output should
contain every second element from the input list. . . and the n-th list in
the output should contain every n-th element from the input list-}
skips :: [a] -> [[a]]
skips xs = [cuts xs n | n <- [1..length xs]]
  where cuts xs n = [x | (x,k) <- zip xs [0..], k `mod` n == (n-1)]


{- 2
A local maximum of a list is an element of the list which is strictly
greater than both the elements immediately before and after it.
Find all the local maxima in the input list and return them in order. -}
localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs) = if a < b && b > c then b:localMaxima (c:xs) else localMaxima (b:c:xs)
localMaxima _ = []

{- 3
takes as input a list of Integer s between 0 and 9 (inclusive),
and outputs a vertical histogram showing how many of each number
were in the input list.

    histogram [1,1,1,5] ==
    *
    *
    *   *
    ==========
    0123456789
-}
histogram :: [Integer] -> String
histogram = hDraw . fillBuckets

fillBuckets :: [Integer] -> [Integer]
-- returns number of hits in a bucket given by index.
fillBuckets = addZeros 0 . counts 
  where counts = map (\x -> (head x, toInteger $ length x)) . groupWith id

addZeros :: Integer -> [(Integer, Integer)] -> [Integer] 
addZeros n s@(x:xs) = let az = addZeros (n+1) in
  if n == fst x then (snd x):(az xs) else 0:(az s)
addZeros n [] = if n <= 9 then 0:(addZeros (n+1)) [] else []

pillars n xs = [if n < x then '*' else ' ' | x <- xs ]

hDraw xs = concat $ intersperse "\n" $ [pillars n xs | n <- reverse [0..maximum xs]] ++
           ["==========", "0123456789"]
