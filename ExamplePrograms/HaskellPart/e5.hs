--guards

f a b c
  | a > b && a > c = a
  | b > c          = b
  | otherwise      = c

{-
  A guard is a boolean expression which, if true, means the expression to
  its right is the value of the function.  If, false, continue down 
    to next guard.

The function above returns the max of three.
-}
