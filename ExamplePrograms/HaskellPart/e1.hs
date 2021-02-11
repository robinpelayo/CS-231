--function definition
--example 1, will not compile

F x y = x + 2 * y

{-
Error message:

e1.hs:4:1: error: Not in scope: data constructor ‘F’
  |
4 | F x y = x + 2 * y
  | ^
Failed, no modules loaded.

-}

-- Not very helpful
-- Haskell thins F is a data constructor, which does start with upper case.
-- functions start with lower case!
