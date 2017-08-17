module List where

infixr 5 :::  
data List a = Nil | a ::: (List a) deriving (Show, Eq, Read, Ord)


--List can be constructed as
a = 1 ::: 2 ::: Nil
b = 5 :::10 ::: Nil


-- Function Implementation

head' (a ::: b) = a

tail' (a ::: b) = b


-- Operator to concate to List
infixr 5 +++
Nil +++ ys = ys
(x:::xs) +++ ys = x ::: (xs +++ ys )


--Converts a single element to List type
toList n = n ::: Nil


map' f Nil = Nil
map' f (x:::xs) = (toList (f x)) +++ map' f xs


filter' f Nil = Nil
filter' f (x:::xs) 
  | f x = (toList x) +++ xs'
  | otherwise = xs'
  where xs' = filter' f xs






-- creates a range from a to b
rng' a b xs
  | a > b = xs
  | otherwise = (toList a) +++ (rng' (a+1) b xs)  

infixr 5 ...
a ... b = to' a b Nil


-- Some Mathematical Functions
sum' Nil = 0
sum' (x:::xs) = x + sum' xs

prod' Nil = 1
prod' (x:::xs) = x * prod' xs

len' Nil = 0
len' (x:::xs) = 1 + (len' xs)



