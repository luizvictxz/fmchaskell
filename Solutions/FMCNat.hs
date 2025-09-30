{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise, String
    )
import GHC.Natural (Natural)
import Graphics.Win32 (FaceName, nOTSRCCOPY)

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show :: Nat -> String
    show O = "O"
    show (S k) = "S" ++ show k

instance Eq Nat where

    (==):: Nat -> Nat -> Bool
    (==) O O = True
    (==) (S n) (S m) = n == m
    (==) _ _ = False

instance Ord Nat where

    (<=):: Nat -> Nat -> Bool
    (<=) O _ = True
    (<=) (S _) O = False
    (<=) (S n) (S m) = n <= m


    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min:: Nat -> Nat -> Nat
    min _ O = O
    min O _ = O
    min (S n) (S m) = S(min n m)

    max:: Nat -> Nat -> Nat
    max n O = n
    max O n = n
    max (S n) (S m) = S(max n m)


----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero (S _) = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S n) = odd n

odd :: Nat -> Bool
odd O = False
odd (S n) = even n


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) n O = n
(<+>) n (S m) = S((<+>) n m)

infixl 6 <+>
-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus O _ = O
monus n O = n
monus (S n) (S m) = monus n m

(<->) :: Nat -> Nat -> Nat
(<->) = monus

infixl 6 <->

-- multiplication
times :: Nat -> Nat -> Nat
times n O = O
times n (S m) = n <+> times n m

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

infixl 7 <*>

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow _ O = O
pow O _ = O
pow n (S m) = n <*> (pow n m)

exp :: Nat -> Nat -> Nat
exp = pow

(<^>) :: Nat -> Nat -> Nat
(<^>) = pow

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) O _ = O
(</>) _ O = undefined
(</>) x y = 
    case x < y of
        True -> O
        False -> S((x <-> y) / y)

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) _ O = undefined
(<%>) x y = 
    case x < y of
        True -> x
        False -> (x <-> y) % y

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv = undefined

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) = undefined

divides = (<|>)


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist = undefined

(|-|) = dist

factorial :: Nat -> Nat
factorial = undefined

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg = undefined

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat = undefined

fromNat :: Integral a => Nat -> a
fromNat = undefined


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = undefined
      | otherwise = undefined

