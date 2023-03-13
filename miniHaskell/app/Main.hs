
module Main where

import System.IO
import System.Console.Isocline

import Exp
import Parsing
import Printing
import REPLCommand
import System.Exit (exitSuccess)
import Text.Parsec (parse)
import Parsing (testParse)

main :: IO ()
main = do 
        p <- readline "miniHaskell>"
        command <- return (testParse replCommand p)
        case command of 
            Quit -> exitSuccess
            Load f -> main 
            Eval e -> do 
                        ex <- return (testParse expr e) 
                        s <- return (showExp ex )
                        putStrLn s  
                        main 


