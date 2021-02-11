import System.Exit
import System.IO


getInput = do
    putStrLn "Enter a letter: "
    input <- getLine
    if input == "g"
         then putStrLn "gg"
         else exitWith (ExitFailure 5)
    
