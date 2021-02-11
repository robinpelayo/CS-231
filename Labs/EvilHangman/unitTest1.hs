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
	getLargestFamRec
-}
getLargestFamRec :: [WordFamily] -> WordFamily -> Int -> WordFamily
getLargestFamRec [] x _ = x
getLargestFamRec (f:fs) currFam famLen
    | famLen < ( (\(_,x,_) -> x) f ) = getLargestFamRec fs f ( (\(_,x,_) -> x) f )
    | otherwise = getLargestFamRec fs currFam famLen

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
--mkFamList [] (w:ws) guess pat = (mkFamList [( (makePattern guess w pat ), 1, [w])] ws guess pat)
mkFamList ((p,l,wl):fs) (w:ws) guess pat 
    | ( makePattern guess w pat ) == p = (p, l + 1, w:wl) : (mkFamList fs ws guess pat)
    | otherwise = (p,l,wl) : (mkFamList fs (w:ws) guess pat)




















