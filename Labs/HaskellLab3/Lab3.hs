{-	Robin Pelayo
	CS-231
	Haskell Lab 3
-}
import Data.CaseInsensitive --for mk function
import Data.Char --for isAlpha
import System.IO 
import System.Environment -- for getArgs function
import Data.List

{-
	rpel function definition

	This function takes a string as input, replaces all non-alphabetic charicters
	with white space, and returns it as a string. The String processed is the entire
	input file that the user wants spell checked. This function is recursive and uses 
	"isAlpha" imported from Data.Char to check each charicter to see if it is alphabetic.

	Psudocode:
	Base case input string is empty list of chars return empty list
	If input string is not empty check the first element of the list to see if it is alphabetic,
		If so append this to recursive call of the rest of the list
		If not append ' ' to recusive call of the rest of the list
-}
repl :: String -> String
repl [] = []
repl (x:xs)
    | isAlpha x = x : repl xs
    | otherwise = ' ' : repl xs

{-
	quick sort function definition

	This function will sort a list of input strings and remove any duplicate strings
	ignoring case.
-}
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = (quickSort [z | z <- xs, (mk z) < (mk x)]) ++ [x] ++ (quickSort [y | y <- xs, (mk y) > (mk x)])

{-
	compare function definition

	This function will take two lists of strings as input and compare them. The first string 
	represents the input list of words and the second is the dictionary string.
	If the input string is empty return the empty string
	If the input string is not empty, but the dictionary is append " is not spelled correctly." to the 
	first word of the input string and append this to the result of the recursive call passing the rest of the 
	list and the empty dictionary string.
	If the input string and dictionary are both not empty process both.
		if the first word of input (ignoring case) is bigger than the dictionart word (ignoring case), read further in dictionary
		if the first word of input (ignoring case) is equal to the dictionart word (ignoring case), combine first word with " is 
		spelled correctly." and append this to the front of the result of the recurive call to comp passing the rest of both the
		dictionary and input list.
		if the first word of input (ignoring case) is less than the dictionart word (ignoring case), combine first word with " is 
		not spelled correctly." and append this to the front of the result of the recurive call to comp passing the rest of the 		input string and the entire dictionary list. 
-}
comp [] _ = []
comp (x:xs) [] = (x ++ " is not spelled correctly.") : comp xs []
comp (x:xs) (y:ys)
    | (mk x) > (mk y) = comp (x:xs) ys
    | (mk x) == (mk y) = (x ++ " is spelled correctly.") : comp xs ys
    | (mk x) < (mk y) = (x ++ " is not spelled correctly.") : comp xs (y:ys)

{-
	main controller function
-}
main = do
    [inputFile, dictionary, outputFile] <- getArgs --get command line arguments
    outHandle <- openFile outputFile WriteMode     --create handle to output file
    
    hPutStrLn outHandle "                Results of Spell Check" --insert title in output file
 
    input <- readFile inputFile     --Input file read into one String
    diction <- readFile dictionary  --Dictionary file read into one string
    
    let lexOutput = (words.repl) input --output of repl sent to words so the input string is split at ' ' spaces
    let sortOutput = quickSort lexOutput --output of the list of words is sent to the qickSort function to remove duplicates and sort
    let spellCheckOut = unlines ( comp sortOutput (words diction) ) 
    --Store the result of the comp function which is sent the sorted unique list of words and the dictionary string split up into words. 
    --The output of the comp function is sent to unlines to ensure each word + " is or is not spelled correctly." is placed on a newline. 
    hPutStr outHandle spellCheckOut --output the checked list of words into utput file using hPutStr
    hClose outHandle --Close the output file 

