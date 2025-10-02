{-# LANGUAGE GADTs #-}

module ExList where

import Prelude hiding
    ( (.) , ($)
    , flip , curry , uncurry
    , iterate
    )

-- use your mind to infer the types, don't cheat!

-- curry takes a "traditional" binary function
-- and returns its currified version
curry :: ((a, b) -> c) -> (a -> (b -> c))
curry f n m = f (n, m) 

-- uncurry takes a currified function
-- and returns its "traditional" binary version
uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (n, m) = f n m

-- flip takes a (currified) binary function
-- and returns one that behaves the same but takes its arguments in the opposite order
flip::(a-> b-> c) -> (b-> a-> c)
flip f b a = f a b

-- (.) takes two composable functions and returns their composition
(.)::((b -> c) -> (a -> b))  -> (a -> c)
(f . g) x  = f (g x)  
-- (.>) is composition but in diagramatic notation (should be ; but Haskell forbids)
(.>) = flip (.)

-- ($) takes a function and a suitable argument and applies the function to the argument
($):: (a -> b) -> a -> b
f $ a = f a
-- think: why would we ever want that?

-- iterate: figure it out by its type
iterate :: (a -> a) -> a -> [a]
iterate f a = a : iterate f (f a)

-- orbit
orbit = flip iterate

