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

-- 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

{- n-th element in the stream (assuming the first element
corresponds to n is the largest power of 2 which evenly divides n. -}
rulerDumb :: Stream Integer
rulerDumb = Stream (findNumber 1 1) (makeRuler 2) where
  makeRuler :: Integer -> Stream Integer
  makeRuler n = Stream (findNumber n $ maxPower n) (makeRuler $ n+1)
  maxPower :: Integer -> Integer
  maxPower n = truncate $ (log $ fromInteger n) / log 2.0
  findNumber n p = if n `mod` (2 ^ p) == 0 then p else findNumber n $ p - 1


streamTwist :: Stream a -> Stream a -> Stream a
streamTwist xs (Stream y ys) = Stream y $ streamTwist ys xs

{- Same as ruleDumb but does not do any math beyond addition. -}
ruleSmart :: Stream Integer
ruleSmart = rule 0 where
  rule n = Stream n (streamTwist (streamRepeat n) (rule (n+1)))


-- 6 infinite polynomes

x :: Stream Integer
x = Stream 0 $ Stream 1 (streamRepeat 0)

instance Num a => Num (Stream a) where
  fromInteger n = Stream (fromInteger n) $ streamRepeat 0
  negate (Stream v rest) = Stream (-v) $ negate rest 
  (+) (Stream v1 rest1) (Stream v2 rest2) = Stream (v1 + v2) $ (+) rest1 rest2
  (*) (Stream v1 rest1) s2@(Stream v2 rest2) = Stream (v1 * v2) (v1 *- rest2 + rest1 * s2) where
    scalar *- (Stream a0 rest) = Stream (scalar * a0) (scalar *- rest)

(*-) scalar (Stream a0 rest) = Stream (scalar * a0) (scalar *- rest)

instance Num a => Fractional (Stream a) where
  (/) (Stream a0 a_rest) (Stream b0 b_rest) = q where
    q = undefined -- Stream (a0 / b0) (1/b0 *- (a_rest - q * b_rest))
    -- have to use the same haed + uncomputed-yet recursive call technique.


-- 7: matrices

{- Matrix2 a00 a01 a10 a11 in
  /a00 a01\
  \a10 a11/
-}
data Matrix2 = Matrix2 Integer Integer Integer Integer 

instance Num Matrix2 where
  fromInteger n = Matrix2 n 0 0 0
  negate (Matrix2 x y z t) = Matrix2 (-x) (-y) (-z) (-t)
  (+) (Matrix2 a00 a01 a10 a11) (Matrix2 b00 b01 b10 b11) =
    Matrix2 (a00 + b00) (a01 + b01) (a10 + b10) (a11 + b11)
  (*) (Matrix2 a00 a01 a10 a11) (Matrix2 b00 b01 b10 b11) =
    Matrix2 c00 c01 c10 c11  where
      c00 = (a00 * b00 + a01 * b10) 
      c01 = (a00 * b01 + a01 * b11) 
      c10 = (a10 * b00 + a11 * b10)
      c11 = (a10 * b01 + a11 * b11)

instance Show Matrix2 where
  show (Matrix2 a00 a01 a10 a11) = "Matrix 2 (" ++ show a00 ++ ", " ++ show a01 ++ " | " ++ 
                                                   show a10 ++ ", " ++ show a11 ++ ")"

fib4 :: Integer -> Integer
fib4 n = x where Matrix2 x _ _ _ = (Matrix2 1 1 1 0) ^ n
