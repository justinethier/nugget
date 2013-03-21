{- |
Module      : Main
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Portability : portable

An experimental port of the 90 minute scheme compiler to Haskell.
-}

module Main where
import qualified Language.Scheme.Core as LSC
import qualified Language.Scheme.Primitives as LSP
import Language.Scheme.Types     -- Scheme data types
-- import qualified Language.Scheme.Variables -- Scheme variable operations
import Control.Monad.Error
import qualified System.Exit
import System.IO
import System.Environment

main :: IO ()
main = do 
    args <- getArgs
    case (null args) of
        True -> showBanner
        _ -> do compileFile $ head args
--            result <- (LSC.runIOThrows $ liftM show $ compileFile $ head args)
--            case result of
--                Just errMsg -> putStrLn errMsg
--                _ -> putStrLn "TODO"

showBanner :: IO ()
showBanner = putStrLn "Usage: hnc filename"

compileFile :: String -> IO ()
compileFile filename = do
    ast <- loadFile filename
    System.Exit.exitSuccess

loadFile :: String -> IO [LispVal]
loadFile filename = do
    result <- runErrorT $ LSP.load filename
    case result of
        Left err -> do
            putStrLn $ show err
            System.Exit.exitFailure
        Right ast -> do
            putStrLn "-------------------------- AST:"
            putStrLn $ show ast
            return $ ast
