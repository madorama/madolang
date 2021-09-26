module Madlib.Operator where

{-# INLINE (|>) #-}
infixl 1 |>
(|>) :: a -> (a -> b) -> b
a |> f = f a
