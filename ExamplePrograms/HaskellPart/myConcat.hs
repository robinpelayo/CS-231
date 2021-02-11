myConcat [] = []
myConcat [[]] = []
myConcat (x:xs) = x ++ myConcat xs
