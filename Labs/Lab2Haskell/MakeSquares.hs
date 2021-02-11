{- 	Robin Pelayo
	CS-231
	Haskell - Lab 2
	Professor Vineyard
-}
module MakeSquares where

{-
	stringToInt function definition

	This function takes a list of Strings as input and 
	outputs the same list of strings converted to Ints.
	This function depends on the predefined function "map"
	which maps the first argument (the name of another function) 
	onto the second argument (in this case a list).

	The function "read" is used as the first argument of the "map" 
	function. Read will cast Strings to another type. The use of 
	map and read together allows the conversion of a list of strings 
	to be converted to a list of Ints. The second input argument of
	of map is not specified here because it simply uses the input
	argument list of Strings
-}
stringToInt :: [String] -> [Int]
stringToInt = map read

{-
	solveRec function definition
	
	This function takes a list of two Ints, "nums", that represents the dimentions of a rectangle.
	The first argment represents the length and the second is the width. The function outputs
	another list of Ints. Each Int represents the length of one side of a square that the 
	rectangle can be broken down into. The lengths are ordered largest to smallest.

	This recursive function has a base case of when the length and width are equal to eachother.
	If the length and width of the rectangle are the same a list of one of the dimentions is 
	output. If the length is bigger than the width the width is appended to the result of the 
	recursive call. This call takes the width as its first input and the length minus the width
	as its second. Lastly, if the width is bigger than the length this time the length is appended
	to the recursive call. Similarly, this call takes the length as input and the width minus the 
	length as the second argument.
-}
solveRec :: [Int] -> [Int]
solveRec nums
  | (head nums) == (last nums) = [(head nums)]
  | (head nums) > (last nums) = (last nums): solveRec [(last nums),((head nums)-(last nums))]
  | (head nums) < (last nums) = (head nums): solveRec [(head nums), ((last nums) - (head nums))]
  
{-
	stringList function definition
	
	This function takes a string as input and outputs a list of strings.
	
	Example "My name is Robin" -> stringList -> ["My", "name", "is", "Robin"]

	This function uses the "words" function to split up the input string using 
	the " " (blank space) as a delimeter. 
-}
stringList :: String -> [String]
stringList str = words str

{-
	solve function definition

	This function is the controller function that only calls all of the 
	supporting funcitions. 

	It is set-up such that the input string, "s", is fed into stringList
	and the output of stringList is the input of stringToInt whos output is 
	fed into the input of solveRec whos output is then used as the output
	of the original solve function. 
-}
solve :: String -> [Int]
solve s = solveRec $ stringToInt $ stringList s



