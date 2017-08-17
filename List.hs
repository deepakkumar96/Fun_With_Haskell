module List where

infixr 5 :::  
data List a = Nil | a ::: (List a) deriving (Show, Eq, Read, Ord)

{-
  	Implemented Function :
	1.  head'
	2.  tail'
	3.  (+++)    -- To concate two List
	4.  range'   -- To create a range of numbers from 1 to n
	5.  (...)    -- Opeartor to create range(A syntatic sugar for range')
	6.  toList   -- Convert a single element to type List
	7.  map'
	8.  filter'
	9.  sum'
	10. prod'
	11. len'	
-}


--List can be constructed as
a = 1 ::: 2 ::: Nil
b = 3 ::: 4 ::: Nil
c = a +++ b


-- Functions

head' :: List a -> a
head' (a ::: b) = a


tail' :: List a -> List a
tail' (a ::: b) = b


(+++) :: List a -> List a -> List a
infixr 5 +++
Nil +++ ys = ys
(x:::xs) +++ ys = x ::: (xs +++ ys )


range' :: (Num a, Ord a) => a -> a -> List a -> List a
range' a b xs
  | a > b = xs
  | otherwise = (toList a) +++ (range' (a+1) b xs)  


(...) :: (Num a, Ord a) => a -> a -> List a
infixr 5 ...
a ... b = range' a b Nil


toList :: a -> List a
toList n = n ::: Nil


map' :: (a -> t) -> List a -> List t
map' f Nil = Nil
map' f (x:::xs) = (toList (f x)) +++ map' f xs


filter' :: (a -> Bool) -> List a -> List a
filter' f Nil = Nil
filter' f (x:::xs) 
  | f x = (toList x) +++ xs'
  | otherwise = xs'
  where xs' = filter' f xs


sum' :: Num a => List a -> a
sum' Nil = 0
sum' (x:::xs) = x + sum' xs


prod' :: Num a => List a -> a
prod' Nil = 1
prod' (x:::xs) = x * prod' xs


len' :: Num a => List t -> a 
len' Nil = 0
len' (x:::xs) = 1 + (len' xs)



------ EXAMPLE => "sum of squares of odd numbers between 1 100" using this List implementation

sumOfSqrs = sum' $ map' (^2) $ filter' odd $ 1...100






