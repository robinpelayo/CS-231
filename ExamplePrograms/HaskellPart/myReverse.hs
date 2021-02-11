myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = (last xs) : myReverse (init (x:xs) )
