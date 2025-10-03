{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = undefined
head (x:_) = x 

tail :: [a] -> [a]
tail [] = []
tail (_:xs) = xs

null :: [a] -> Bool
null [] = True
null _ = False

length :: Integral i => [a] -> i
length [] = 0
length (_:xs) = 1 + length xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 0
product (x:xs) = 
  if null xs
    then x
    else x * product xs


reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
(++) [] xs = xs 
(++) (x:xs) ys = x:(xs++ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x xs = xs ++ [x]

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = undefined
minimum [x] = x 
minimum (x:xs) = min x (minimum xs)


-- maximum :: Ord a => [a] -> a
maximum:: Ord a => [a] -> a
maximum [] = undefined
maximum [x] = x
maximum (x:xs) = max x (maximum xs)

-- take
take:: Integral a => a -> [a] -> [a]
take x _ | x <= 0 = []
take x ys | x > length ys = error "Number's big"
take _ [] = []
take x (y:ys) = y: take (x-1) ys

-- drop
drop:: Integral a => a -> [a] -> [a]
drop x xs | x <= 0 = xs
drop x xs | x > length xs = error "Number's big"
drop _ [] = []
drop x (_:ys) = drop (x-1) ys

-- takeWhile
takeWhile:: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x:xs) = 
  if f x
    then x:takeWhile f xs
    else takeWhile f xs 

-- dropWhile
dropWhile:: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f (x:xs) =
  if f x
    then dropWhile f xs
    else x:dropWhile f xs

    
-- tails
tails:: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) =  (x:xs) : tails xs

-- init
init:: [a] -> [a]
init [] = error "no support to empty list"
init [_] = []
init (x:xs) = x : init xs 

-- inits
inits:: [a] -> [[a]]
inits [] = [[]]
inits xs = xs : inits (init xs)

-- subsequences
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = 
  let rest = subsequences xs
  in rest ++ map (x:) rest

-- any
any:: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f (x:xs)
  | f x = True
  | otherwise = any f xs

-- all
all:: (a -> Bool) -> [a] -> Bool
all _ [] = True
all f (x:xs)
  | f x = all f xs
  | otherwise = False


-- and
and:: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

-- or
or:: [Bool] -> Bool
or [] = True
or (x:xs) = x || or xs

-- concat
concat:: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

elem:: Eq a => a -> [a] -> Bool
elem x = any (== x)

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem':: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
  | x == y = True
  | otherwise = elem' x ys

-- (!!)
(!!):: Integral i => [a] -> i -> a
xs !! i 
  | i < 0 || i > length xs = error "out range"
[] !! _ = error "not support to empty list"
(x:_) !! 0 = x
(_:xs) !! a = xs !! (a-1)

-- filter
filter:: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (n:ns) =
  if p n
    then n : filter p ns
    else filter p ns

-- map
map:: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- cycle
cycle:: [a] -> [a]
cycle [] = error "not support to empty list"
cycle xs = xs ++ cycle xs

-- repeat
repeat:: a -> [a]
repeat x = x : repeat x

-- replicate
replicate:: (Integral i, Eq i) => i -> a -> [a]
replicate 0 _ = []
replicate i a = a: replicate (i-1) a


-- isPrefixOf
isPrefixOf:: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- isInfixOf
isInfixOf:: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf (x:xs) (y:ys) = isPrefixOf xs (y:ys) || isInfixOf xs ys

-- isSuffixOf
isSuffixOf:: Eq a => [a] -> [a] -> Bool
isSuffixOf xs ys = isPrefixOf (reverse xs) (reverse ys)

-- zip
zip:: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

-- zipWith    
zipWith:: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] _ = []
zipWith f _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- intercalate
intercalate:: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate _ [xs] = xs 
intercalate xs (ys:yss) = ys ++ xs ++ intercalate xs yss 

-- nub
nub:: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) = x : nub (filter (x /=) xs)

-- splitAt
splitAt :: (Integral i) => i -> [a] -> ([a], [a])
splitAt i xs | i <= 0 || i > length xs = ([], xs)
splitAt _ [] = ([], [])
splitAt i (x : xs) =
  let (leftPart, rightPart) = splitAt (i - 1) xs
   in (x : leftPart, rightPart)

-- break
break :: (a -> Bool) -> [a] -> ([a], [a])
break _ [] = ([], [])
break p (x : xs)
  | p x = ([], x : xs)
  | otherwise =
      let (ys, zs) = break p xs
       in (x : ys, zs)

-- lines
lines :: String -> [String]
lines "" = []
lines s =
  let (firstLine, rem) = break (== '\n') s
   in case rem of
        "" -> [firstLine]
        (_ : afterBreak) -> firstLine : lines afterBreak

-- words
words :: String -> [String]
words s =
  case dropWhile C.isSpace s of
    "" -> []
    s' ->
      let (word, rem) = break C.isSpace s'
       in word : words rem

-- unlines
unlines :: [String] -> String
unlines [] = ""
unlines s = intercalate "\n" s ++ "\n"

-- unwords
unwords :: [String] -> String
unwords = intercalate " "

-- transpose
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose matrix =
  let nonEmptyRows = filter (not . null) matrix
   in if null nonEmptyRows
        then []
        else map head nonEmptyRows : transpose (map tail nonEmptyRows)

-- normalize
normalize :: String -> String
normalize s = filter C.isAlphaNum (map C.toLower s)

-- palindrome
palindrome :: String -> Bool
palindrome s =
  let normS = normalize s
   in normS == reverse normS

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}