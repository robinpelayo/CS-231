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
type WordFamily = (Dashes, FamilySize, [Word])
type Dashes = String
type RemainingLetters = Int
type GuessedLetters = [Char]

{-
	isFifteen
-}
isFifteen :: Int -> Int
isFifteen 15 = 15
isFifteen n = n


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
	getWord function def
-}
getWord :: WordFamily -> Word
getWord wf = (\(_,_,ws) -> (head ws)) wf

{-
	userWin function to tell if the game is over and the user has beat the AI
-}
userWin :: Dashes -> Bool
userWin = not . elem '_'

{-
	gameLoop function 
	function to simulate the game
-}
gameLoop :: [String] -> GuessedLetters -> WordFamily -> Dashes -> Int -> Bool -> IO ()
gameLoop diction gl wordFam pattern remGuesses nOption
    --Check to see if user has won
    | userWin pattern = do
         putStrLn ("You have won! The word was " ++ ( read pattern) ++ ". Would you like to play again? (Y/N)")
         exitWith (ExitFailure 5)
{-
         replayW <- getChar
         putStrLn ""
         let replayW' = toUpper replayW
         if replayW' == "Y"
             then do
                 startReplay diction
             else do
                 putStrLn "You have chosen to quit"
                 exitWith (ExitFailure 5)
         else do
             exitWith (ExitFailure 5)
-}
    | remGuesses == 0 = do
         putStrLn ("You lose! The word was " ++ (getWord wordFam) ++ ". Would you like to play again? (Y/N)")
         exitWith (ExitFailure 5)
{-
         replayL <- getChar
         putStrLn ""
         let replayL' = toUpper replayL
         if replayL' == "Y"
             then do
                 startReplay diction
             else do
                 putStrLn "You have chosen to quit"
                 exitWith (ExitFailure 5)
         else do
             exitWith (ExitFailure 5)
-}
    | otherwise = do
         putStrLn ("Remaining Guesses:    " ++ (show remGuesses) )
         putStrLn ("Guessed Letters:    " ++ gl)
         putStrLn ("Current Word:    " ++ pattern)
         --n Option
         if nOption 
             then putStrLn ("Current Word Family Size: " ++ (show ( (\(_,x,_) -> x) wordFam) ) )
             else return ()
{-
         guess <- newGuess gl
         let gl' = guess `union` gl
         case gl' == gl of
             True -> do putStrLn "You already guesses that!"
                        gameLoop diction gl wordFam pattern remGuesses nOption
             False -> do
                        wordFam' <- mkNewFam wordFam guess 
                        let pattern' = (\(x,_,_) -> x) wordFam'
                        let cheatCheck = compPattern pattern pattern'
                        case cheatCheck of
                            True -> putStrLn "Too bad!" 
                                    gameLoop diction gl' largestFam pattern' (remGuesses - 1) nOption
                            False -> putStrLn "Lucky you!"
                                    gameLoop diction gl' largestFam pattern' remGuesses nOption

-}




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
    let initFamSize = length sortedDiction
    let wordFam = (initialPattern, initFamSize, sortedDiction)
    
    if (args !! 3) == "-n"
        then do 
               putStrLn "-n detected"
               let nSwitch = True
               gameLoop diction [] wordFam initialPattern guesses nSwitch
        else do --Call gameloop
               gameLoop diction [] wordFam initialPattern guesses False
