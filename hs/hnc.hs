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
import Language.Scheme.Types
import qualified Language.Scheme.Variables as LSV
import Control.Monad.Error
import qualified System.Exit
import System.IO
import System.Environment

main :: IO ()
main = do 
    args <- getArgs
    case (null args) of
        True -> showBanner
        _ -> compileFile $ head args

showBanner :: IO ()
showBanner = putStrLn "Usage: hnc filename"

compileFile :: String -> IO ()
compileFile filename = do
    ast <- loadFile filename
    putStrLn "-------------------------- AST:"
    putStrLn $ show ast
--
-- TODO: may want to think about doing code generation first,
-- and just passing in code in the proper format. otherwise
-- it may be more painful later if the cps/closure code needs
-- to be reworked later on...
--
--    astAfterCPS <- cpsConvert ast
--    putStrLn "-------------------------- AST AFTER CPS-CONVERSION:"
--    putStrLn $ show astAfterCPS

    code <- generateCode ast
    putStrLn "-------------------------- C CODE:"
    putStrLn $ show code
    System.Exit.exitSuccess


loadFile :: String -> IO [LispVal]
loadFile filename = do
    result <- runErrorT $ LSP.load filename
    case result of
        Left err -> do
            putStrLn $ show err
            System.Exit.exitFailure
        Right ast -> do
            return $ ast
-- TODO: consolidate common code w/above func
generateCode ast = do
    result <- runErrorT $ codeGenerate ast
    case result of
        Left err -> do
            putStrLn $ show err
            System.Exit.exitFailure
        Right code -> do
            return code

cpsConvert :: [LispVal] -> IO [LispVal]
cpsConvert ast = cps ast []

cps :: [LispVal] -> [LispVal] -> IO [LispVal] 
cps (a : as) acc = cps as [] -- TODO
cps [] result = return result

---------------------------------------------------------------------
-- Free variables
-- TODO: port fv 
freeVars :: 
    --Env -> 
    [LispVal] -> 
    IOThrowsError LispVal

---------------------------------------------------------------------
-- code generation section

codeGenerate :: [LispVal] -> IOThrowsError [String]
codeGenerate ast = do
   let codePrefix = "   \n\
          \TODO: PREFIX \n\
          \PREFIX line 2 "
       codeSuffix = "TODO: SUFFIX"

   cgEnv <- liftIO $ nullEnv -- Local Env for code generation phase
   _ <- LSV.defineVar cgEnv "lambda-todo" $ List []
   _ <- LSV.defineVar cgEnv "lambda-count" $ Number 0


   code <- gen ast []
   return $ [
      "#define NB_GLOBALS \n" , -- TODO: (length global-vars) "\n"
      "#define MAX_STACK 100 \n" , -- could be computed...
      codePrefix ] ++ code ++ [codeSuffix]

gen :: [LispVal] -> [String] -> IOThrowsError [String]
gen (a : as) acc = gen as [] -- TODO
gen [] result = return result
