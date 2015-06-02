-- Homework 1: credit cards
{-
Double the value of every second digit beginning from the right.
That is, the last digit is unchanged; the second-to-last digit is doubled.

Add the digits of the doubled values and the undoubled digits from the original number.
For example, [2,3,16,6] becomes 2+3+1+6+6 = 18

Calculate the remainder when the sum is divided by 10. If 0, card is valid.
-}

import Data.Char

sumDigits :: Integral n => n -> n
sumDigits 0 = 0
sumDigits n = rem + sumDigits res
  where (res, rem) = divMod n 10


dupEvens :: Integral n => [n] -> [n]
dupEvens ns = dupEvensHd $ reverse ns

dupEvensHd (x1:x2:xs) = x1 : (x2 * 2) : dupEvensHd xs
dupEvensHd whatever = whatever

addUp ns = sum $ [sumDigits n | n <- ns]

isValidCC :: String -> Bool
isValidCC s = (addUp $ dupEvens $ map digitToInt s) `mod` 10 == 0


-- Hanoi tower: never put a larger disk on top of smaller

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 src tmp dst = [(src, dst)]
hanoi n src tmp dst = hanoi (n - 1) src dst tmp ++ [(src, dst)] ++ hanoi (n - 1) tmp src dst

{- too lazy yet
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 src t1 t2 dst = [(src, dst)]
hanoi4 n src t1 t2 dst =
-}
