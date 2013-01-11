{- |
Module      : Main
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Portability : portable

An experimental port of the 90 minute scheme compiler to Haskell.
-}

module Main where
import qualified Language.Scheme.Core      -- Scheme Interpreter
import qualified Language.Scheme.Types     -- Scheme data types
import qualified Language.Scheme.Variables -- Scheme variable operations
import Control.Monad.Error
import System.IO
import System.Environment

main :: IO ()
main = do 
    args <- getArgs
    case (null args) of
        True -> showBanner
        _ -> compileFile $ head args

showBanner :: IO ()
showBanner = putStrLn "TODO: banner"

compileFile :: String -> IO ()
compileFile filename = putStrLn $ "TODO: " ++ filename
