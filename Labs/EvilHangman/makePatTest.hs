{-
	makePattern function
-}
makePattern:: Char -> String -> String
makePattern _ [] = []
makePattern g (l:ls)
    | g == l = g : makePattern g ls
    | otherwise = '_' : makePattern g ls
