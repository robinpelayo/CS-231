{-
	Robin Pelayo, Evan Casey
	CS-231 Evil Hangman 

-}
import System.IO
import System.Environment
import Data.Char
import Data.List
import System.Exit
import System.Directory
import Prelude hiding (Word)


--Type definition
type Word = String
type FamilySize = Int
type WordFamily = [(Dashes, FamilySize, [Word])]
type Dashes = String
type RemainingLetters = Int
type GuessedLetters = [Char]

getWordFamilyList :: Char -> Dashes -> [Word] -> [WordFamily] -> [WordFamily]
getWordFamilyList guess pat (w:ws) famlist 
    | guess not `elem` w = 



{-
	getFamilySize function
-}
getFamilySize :: WordInfo -> Int
getFamilySize ws = length  ws


{-
	getUnknowns function
-}
getUnknowns :: Dashes -> Int
getUnknowns = sum . map (\x -> if x == '_' then 1 else 0)

{-
{-
	getGuesses function
-}
getGuesses = do
    putStrLn "\n Enter number of guesses: "
    input <- getLine
    let gs = (read input :: Int)
    if gs < 1 
         then do
             putStrLn "Error invalid number of guesses."
             exitWith (ExitFailure 5)
         else if gs > 15
                 then do
                     putStrLn "Number of guessess set to 15."
                     15
                 else gs


getWordLength diction = do
    putStrLn "\n Enter length of word: "
    input <- getLine
    let wl = (read input :: Int)
    if newDiction diction wl == []
         then do
             putStrLn "Error invalid word length."
             exitWith (ExitFailure 5) 
         else wl
-}
{-
	Function to get the user's new letter guess
	Outputs the guessed letter 
-}
newGuess :: [Char] -> IO [Char]
newGuess gs = do
    putStrLn "Guess a Character"
    guess <- getChar
    putStrLn ""
    let guess' = [toUpper guess]
    return $ guess' `union` gs 

{-
	Function to test if input string is "-n"
	Outputs true if so and false if not
-}
nOption str
    | str == "-n" = True
    | otherwise = False

{-
	userWin function to tell if the game is over and the user has beat the AI
-}
userWin :: Dashes -> Bool
userWin = not . elem '_'

{-
	getPattern dfinition

-}
getPattern :: GuessedLetters -> Word -> Dashes
getPattern gl = map (\x -> if x `elem` gl then x else '_')

{-
{-
	Function to give a string the same length as input number. 
	The string will contain only "_"
-}
dashes length
    | length == 0 = []
    | otherwise = "_" ++ dashes (length - 1)
-}

{-
	Function to check if the input list of words contains a word that has length equal to input number
	This function also changes the list of words to be upper case
-}
newDiction :: [String] -> Int -> [String]
newDiction [] y = []
newDiction (x:xs) y
    | length x == y = (map toUpper x ) : newDiction xs y
    | otherwise = newDiction xs y

{-
	updateWorList function
-}
updateWordList :: GuessedLetters -> WordInfo -> WordInfo
updateWordList _ [] = []
updateWordList gl ((word, pat, _):ws) = (word, pat', count) : updateWordList gl ws
    where pat' = getPattern gl word
          count = getUnknowns pat'

--TODO write getPattern and getUnknowns

{-
	getWord function def
-}
getWord :: WordInfo -> Word
--getWord [] = error "this should never happen"
getWord ws = (\(w,_,_) -> w) (head ws)

{-
	newList
-}
newList ::Int -> WordInfo -> WordInfo
newList maxCount = filter (\(_,_,count) -> count == maxCount)
{-
	findPattern function
-}
findPattern :: Dashes -> WordInfo -> Bool
findPattern updateDashes wi = updateDashes `elem` [ d | (_,d,_) <- wi ]

{-
	largestCount function
-}
largestDash :: WordInfo -> Int
largestDash = maximum . map (\(_,_,dash) -> dash)

{-
	removeNonPattern function
-}
removeNonPattern :: Dashes -> WordInfo -> WordInfo
removeNonPattern pn = filter (\(_,d,_) -> d == pn)

{-
	getNewPattern function
-}
getNewPattern :: Dashes -> WordInfo -> (WordInfo, Dashes)
getNewPattern pt wi = (match, pt)
    where maxDash = filter (\(_,_,c) -> c == maxCount) wi
          --TODO check program documentation.  
          maxCount = largestDash wi
          --pick first pattern we come across
          pattern = head [pt | (_,pt,_) <- maxDash]
          --filter to remove anything without pattern
          match = filter (\(_,pt,_) -> pt == pattern) maxCount

{-
	isFifteen
-}
isFifteen :: Int -> Int
isFifteen 15 = 15
isFifteen n = n

{-
	gameLoop function 
	function to simulate the game
-}
gameLoop :: [String] -> GuessedLetters -> WordInfo -> Dashes -> Int -> Bool -> IO ()
gameLoop diction gl wordList pattern remGuesses nOption
    --If there are no more _ in the pattern, we have found a word
    --TODO Play again
    | userWin pattern = do
        putStrLn ("Congratulations! The word was " ++ (read pattern) ++ ".")
        exitWith (ExitFailure 5)
	--When you run out of guesses, the game is over and the player has lost	
    | remGuesses == 0 = do
        putStrLn ("Game over. The word was " ++ (getWord wordList) ++ ".")
        exitWith (ExitFailure 5)
	--Game is in progress
    | otherwise = do
	     --list the current game info
        putStrLn ("Remaining guesses: " ++ (show remGuesses) )
        putStrLn ("Guessed Letters:   " ++ gl)
        putStrLn ("Current Word:      " ++ pattern)
        --n Option
        if nOption 
            then putStrLn ("Current word family size: " ++ ( show (getFamilySize wordList) ))
            else return()
        --Get new letter guess
        gl' <- newGuess gl
        case gl' == gl of
            True -> do putStrLn "You already guessed that!"
                       gameLoop diction gl wordList pattern remGuesses nOption
            False -> do
                let wordList' = updateWordList gl' wordList
                let cheatCheck = findPattern pattern wordList'
                case cheatCheck of
                    True -> do putStrLn "cheating"
                               let wi = removeNonPattern pattern wordList'
                               gameLoop diction gl' wordList pattern (remGuesses - 1) nOption
                    False -> do putStrLn "can't cheat"
                                let (wi, pt) = getNewPattern pattern wordList'
                                gameLoop diction gl' wi pt (remGuesses -1) nOption
					
{-
	main
-}
main = do
    args <- getArgs --get command line arguments
    if length args > 4 || length args < 3 
        then do 
            putStrLn "usage: ./Hangman dictionary_name length_of_word number_of_guesses"
            exitWith (ExitFailure 5) 
        else return()
    haveDiction <- doesFileExist (args !! 0)
    if not haveDiction 
        then do 
            putStrLn "Dictionary file does not exist."
            exitWith (ExitFailure 5) 
        else return()
    if (read (args !! 1)) < 0 || (read (args !! 2)) < 1
        then do
            putStrLn "Word length must be positive. Number of guesses must be more than 1."
            exitWith (ExitFailure 5)
        else return()
    
    indiction <- readFile (args !! 0) --read dictionary in as string and break up into words
    let diction = words indiction
    let inguesses = read (args !! 2) :: Int
    let guesses = isFifteen inguesses
    let wordLength = (args !! 1)
    if guesses == 15
        then do
            putStrLn "Number of guesses set to 15."
            return ()
        else do return()
    
    --Check to see if a word in the dictionary has same length as input length
    let sortedDiction = newDiction diction (read wordLength)
    if sortedDiction == []
         then do
             putStrLn ("Error no word of length " ++ wordLength)
             exitWith (ExitFailure 5)
    else return()
    
    --Check for -n option
    --let nSwitch = nOption familySize
    
    putStr "                   Evil Hangman"
    
    let initialPattern = replicate (read wordLength) '_'
    let wordList = map (\x -> (x, initialPattern, (read wordLength))) sortedDiction
    
    --Call gameloop
    gameLoop diction [] wordList initialPattern guesses False





