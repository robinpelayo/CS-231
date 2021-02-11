myTake [] _ = []
myTake (x:xs) 1 = [x]
myTake (x:xs) n = x : myTake xs (n - 1)
