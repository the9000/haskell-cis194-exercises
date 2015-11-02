import AParser

-- 1: make Parser a Functor

first :: (a -> b) -> (a, x) -> (b, x)
first f (a, x) = ((f a), x)

instance Functor Parser where
    fmap = undefined
