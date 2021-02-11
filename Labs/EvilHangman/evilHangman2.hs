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
	getWord function def
-}
getWord :: WordFamily -> Word
getWord wf = (\(_,_,ws) -> (head ws)) wf

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
	userWin function to tell if the game is over and the user has beat the AI
-}
userWin :: Dashes -> Bool
userWin = not . elem '_'

{-
	getLargestFamRec
-}
getLargestFamRec :: [WordFamily] -> WordFamily -> Int -> WordFamily
getLargestFamRec [] x _ = x
getLargestFamRec (f:fs) currFam famLen
    | famLen < ( (\(_,x,_) -> x) f ) = getLargestFamRec fs f ( (\(_,x,_) -> x) f )
    | otherwise = getLargestFamRec fs currFam famLen


{-
	compPattern function
-}
compPattern :: Dashes -> Dashes -> Bool
compPattern origPat newPat = origPat == newPat

{-
	Function to give a string the same length as input number. 
	The string will contain only "_"
-}
dashes length
    | length == 0 = []
    | otherwise = "_" ++ dashes (length - 1)

{-
	makePattern function
-}
makePattern:: Char -> Word -> Dashes -> Dashes
makePattern _ [] _ = []
makePattern g (l:ls) (p:ps)
    | g == l = g : makePattern g ls ps
    | otherwise = p : makePattern g ls ps

{-
	getLargestFam
-}
getLargestFam :: [WordFamily] -> WordFamily
getLargestFam (fl:fls) = getLargestFamRec (fl:fls) fl ( (\(_,x,_) -> x) fl )

{-
	mkNewFam function
-}
mkNewFam:: WordFamily -> Char -> WordFamily
mkNewFam wf guess = getLargestFam ( mkFamList []  ( (\(_,_,x) -> x) wf ) guess ((\(x,_,_) -> x) wf ) )

{-
	mkFamList function
-}
mkFamList :: [WordFamily] -> [Word] -> Char -> String -> [WordFamily]
mkFamList [] [] _ _ = []
mkFamList x [] _ _ = x
mkFamList [] (w:ws) guess pat = ( (makePattern guess w pat ), 1, [w]) : (mkFamList [] ws guess pat)
mkFamList ((p,l,wl):fs) (w:ws) guess pat 
    | ( makePattern guess w pat ) == p = (p, l + 1, w:wl) : (mkFamList fs ws guess pat)
    | otherwise = (p,l,wl) : (mkFamList fs (w:ws) guess pat)

{-
{-
        startReplay function
-}
checkInput diction length guesses = do
    if (length < 0) || (guesses < 1)
        then do
            putStrLn "Word length must be positive. Number of guesses must be more than 1."
            exitWith (ExitFailure 5)
        else return ()
    let totGuesses = isFifteen guesses
    if totGuesses == 15
        then do
            putStrLn ("Number of guesses set to 15.")
            return ()
    else do return ()

    let replayDiction = newDiction diction length
    let replayPattern = replicate length '_'
    let replayFam = (replayPattern, length, replayDiction)
    if replayDiction == []
        then do
            putStrLn ("Error no word of length " ++ length)
            exitWith (ExitFailure 5)
    else do
        gameLoop diction [] replayFam replayPattern guesses False
-}
isFifteen guess = do
    if guess > 15
        then do
            putStrLn "Number of guesses set to 15."
            let guesses = 15
            return guesses
        else do 
            let guesses = guess
            return guesses

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
                 putStrLn "You have chosen to play again."
                 putStrLn "What is your desired word length?"
                 replayLength <- getChar
                 putStrLn "How many guesses are allowed?"
                 replayGuesses <- getChar
                 let replayLength' = (read replayLength :: Int)
                 let replayGuesses' = (read replayGuesses :: Int)
                 checkInput diction replayLength' replayGuesses'
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
                 putStrLn "You have chosen to play again."
                 putStrLn "What is your desired word length?"
                 replayLength <- getChar
                 putStrLn "How many guesses are allowed?"
                 replayGuesses <- getChar
                 let replayLength' = (read replayLength :: Int)
                 let replayGuesses' = (read replayGuesses :: Int)
                 checkInput diction replayLength' replayGuesses'
             else do
                 putStrLn "You have chosen to quit"
                 exitWith (ExitFailure 5)
         else do
             exitWith (ExitFailure 5)
-}
    | otherwise = do

         --n Option
         putStrLn ("check 1")
         if nOption
             then do   
                 putStrLn ("check 2")      
                 putStrLn ("Remaining Guesses:    " ++ (show remGuesses) )
                 putStrLn ("Guessed Letters:    " ++ gl)
                 putStrLn ("Current Word:    " ++ pattern) 
                 putStrLn ("Current Word Family Size: " ++ (show ( (\(_,x,_) -> x) wordFam) ) )
             else do
                 putStrLn ("check 3")
                 putStrLn ("Remaining Guesses:    " ++ (show remGuesses) )
                 putStrLn ("Guessed Letters:    " ++ gl)
                 putStrLn ("Current Word:    " ++ pattern) 
         putStrLn ("check 4")
         putStrLn ("Guess a Character")
         guess <- getChar
         putStrLn ""
         let guess' = toUpper guess
         let gl' = ([guess'] `union` gl)
         case gl' == gl of
             True -> do putStrLn "You already guessed that!"
                        putStrLn ("check repeat guess")
                        gameLoop diction gl wordFam pattern remGuesses nOption
             False -> do
                        putStrLn ("check new guess")
                        let wordFam' = mkNewFam wordFam guess 
                        let pattern' = (\(x,_,_) -> x) wordFam'
                        let cheatCheck = compPattern pattern pattern'
                        case cheatCheck of
                            True -> do putStrLn "Too bad!" 
                                       gameLoop diction gl' wordFam' pattern' (remGuesses - 1) nOption
                            False -> do putStrLn "Lucky you!"
                                        gameLoop diction gl' wordFam' pattern' remGuesses nOption

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
    let guess = read (args !! 2) :: Int
    let wordLength = (args !! 1)
    guesses <- isFifteen guess 
    
    --Check to see if a word in the dictionary has same length as input length
    let sortedDiction = newDiction diction (read wordLength)
    if sortedDiction == []
         then do
             putStrLn ("Error no word of length " ++ wordLength)
             exitWith (ExitFailure 5)
    else return()
    
    --Print title
    putStrLn "                   Evil Hangman"
    
    let initialPattern = replicate (read wordLength) '_'
    let initFamSize = length sortedDiction
    let wordFam = (initialPattern, initFamSize, sortedDiction)
    
    if (length args == 4) && ( (args !! 3) == "-n" )
        then do 
               putStrLn "-n detected"
               let nSwitch = True
               gameLoop diction [] wordFam initialPattern guesses nSwitch
        else do --Call gameloop
               gameLoop diction [] wordFam initialPattern guesses False
    

