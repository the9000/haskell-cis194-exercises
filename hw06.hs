-- 1: naive Fibonacci

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


fibs1 :: Integer -> [Integer]
fibs1 0 = [fib 0]
fibs1 n = (fibs1 (n - 1)) ++ [fib n]

-- 2: efficient Fibonacci

-- (extra) Plain recursive, O(n) memory and time
efib :: Integer -> (Integer, Integer)
efib 0 = (undefined, 0)
efib 1 = (0, 1)
efib n = (minus1, minus2 + minus1) where
  (minus2, minus1) = efib (n - 1)

-- (extra) Tail-recursive, O(1) memory, O(n) time
trfib_ :: Integer -> (Integer, Integer) -> (Integer, Integer)
trfib_ 0 acc = acc
trfib_ n (minus2, minus1) = trfib_ (n - 1) (minus1, minus2 + minus1)

trfib :: Integer -> Integer
trfib 0 = 0
trfib n = snd $ trfib_ n (0, 1)

-- Efficient list generation: O(n) memory and time to compute n-th element,
-- except the ++ and reverse operations are O(n) per element :(
fibs2 :: [Integer]
fibs2 = [0, 1] ++ (compute 0 1) where
  compute :: Integer -> Integer -> [Integer]
  compute minus2 minus1 = next:(compute minus1 next) where
    next = minus1 + minus2

-- 3: define Stream

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream a s) = a : streamToList s

instance Show a => Show (Stream a) where
  show s = "Stream " ++ (show $ take 20 $ streamToList s)
  
-- 4

{- A stream endlessly repeating a constant -}
streamRepeat c = Stream c (streamRepeat c)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a s) = Stream (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed rule start = Stream start (streamFromSeed rule (rule start))
