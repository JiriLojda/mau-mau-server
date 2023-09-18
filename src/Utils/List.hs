module Utils.List (
  head,
  last,
) where

import Prelude hiding (head, last)

head :: [a] -> Maybe a
head [] = Nothing
head (x : _) = Just x

last :: [a] -> Maybe a
last [] = Nothing
last [x] = Just x
last (_ : xs) = last xs
