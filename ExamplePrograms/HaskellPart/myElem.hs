myElem _ [] = False
myElem x (y:ys) 
  | x == y = True
  | otherwise = myElem x ys

