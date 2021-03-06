-- Rewite more idiomatically

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

                
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

----

fun1' :: [Integer] -> Integer
fun1' = foldl f1 1 . filter even
  where f1 acc x = acc * (x-2)


fun2' :: Integer -> Integer
fun2' n = fst $ head $ take 1 $ dropWhile ((/= 1) . snd) $ iterate f2 (0, n)
  where f2 (acc, n) = if even n then (acc + n, div n 2) else (acc, 3 * n + 1)


-- outtake

fac1 :: (Integral a) => a -> a
fac1 0 = 1
fac1 x = x * fac1 (x - 1)

fac2 :: (Integral a) => a -> a
fac2 n = fst . head . take 1 . dropWhile ((>0) . snd) $ iterate f (1, n)
  where f (acc, n) = (acc * n, n - 1)


-- fold tree: generate a balanced binary tree from a list of values using foldr

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr addChild Leaf


addChild :: a -> Tree a -> Tree a
addChild a Leaf = Node 0 Leaf a Leaf
addChild a (Node d left v right) = Node new_d new_left v new_right
  where left_d = depth left
        right_d = depth right
        (new_left, new_right) = if left_d <= right_d
                                then (addChild a left, right)
                                else (left, addChild a right)
        new_d = (max (depth new_left) (depth new_right)) + 1
  

depth :: Tree a -> Integer
depth Leaf = -1
depth (Node d _ _ _) = d


printTree :: Show a => Tree a -> String
printTree = pt 0 
pt _ Leaf = ""
pt pad (Node n left v right) = (pt (pad + 1) right) ++ 
                               (take pad $ repeat ' ') ++
                               (show n) ++ " " ++
                               (show v) ++ "\n" ++
                               (pt (pad + 1) left)
             
{- xor fold -}

xor :: [Bool] -> Bool
xor = foldl changer False
  where changer acc value = if value then not acc else acc

{- map via foldr -}

map' :: (a -> b) -> [a] -> [b]
map' f = foldr madd []
  where madd value acc = (f value):acc

{- foldl via foldr -}
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base xs


{- producing primes -}
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [2 * k + 1 | k <- [1..n], isGood k]

isGood k = not $ any id [(i + j + 2 * i * j) == k |
                         j <- [1..(k `div` 2 + 2)], i <- [1..j+1]]

