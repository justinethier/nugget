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
import qualified Data.List as DL
import qualified Data.Set as DS
import qualified System.Exit
import System.IO
import System.Environment
import Debug.Trace

main :: IO ()
main = do 
    args <- getArgs
    case (null args) of
        True -> showBanner
        _ -> compileFile $ head args

showBanner :: IO ()
showBanner = putStrLn "Usage: nugget filename"

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
-- TODO: this is a port of fv, although it needs to filter
--       out primitive functions, maybe other things
-- Also TBD if strings are good enough, since output from gambit
-- seemed to contain more information
freeVars :: 
    -- ? Env -> 
    LispVal -> 
    [LispVal] 
freeVars v@(Atom _) = [v]
freeVars (List (Atom "set!" : v@(Atom _) : rest)) = do
    DS.toList $ DS.union (DS.fromList [v]) 
                         (DS.fromList (freeVars $ List rest))
freeVars (List (Atom "lambda" : List vs : body)) =
    DS.toList $ DS.difference (DS.fromList (freeVars $ List body))
                              (DS.fromList vs)
freeVars (List ast) = do
    let fvs = map (\ l -> DS.fromList $ freeVars l) ast
    DS.toList $ DS.unions fvs
freeVars _ = []

---------------------------------------------------------------------
-- code generation section

codeGenerate :: [LispVal] -> IOThrowsError [String]
codeGenerate ast = do
   let fv = freeVars $ List ast

   cgEnv <- liftIO $ LSC.r5rsEnv -- Local Env for code generation phase
   _ <- LSC.evalLisp cgEnv $ List [Atom "load", String "code-gen.scm"]
   String codeSuffix <- LSV.getVar cgEnv "code-suffix"

   code <- (trace ("fv = " ++ show fv) gen ast [])
   return $ [
      "#define NB_GLOBALS \n" , -- TODO: (length global-vars) "\n"
      "#define MAX_STACK 100 \n" ] -- could be computed...
      --TODO: codePrefix] 
      ++ code ++ [codeSuffix]

-- |A port of (compile-all-lambdas) from "90 minutes"
compileAllLambdas ::
    Env -> 
    LispVal ->
    IOThrowsError [String]
compileAllLambdas env = do
    todo <- LSV.getVar env "lambda-todo"
    isTodo <- LSP.isNull [todo]
    case isTodo of
       Bool True -> return [] 
       _ -> do
        x <- LSP.car [todo]
        caseNum <- LSP.car [x]
        ast <- LSP.cdr [x]
        LSP.cdr [todo] >>= LSV.setVar env "lambda-todo" 
       
        -- TODO: how to differentiate ast and ast-subx???
        --astH <- LSP.car [ast]
        --astT <- LSP.cdr [ast]

        g <- gen 
        rest <- compileAllLambdas env
        return [
            "case " ++ caseNum ++ ": /* " ++ show ast ++ " */\n\n", -- " (object->string (source ast) 60) " */\n\n"

       -- TODO:
       --    (code-gen (car (ast-subx ast))
       --              (reverse (lam-params ast)))

            "\n\n"] ++ rest

gen :: [LispVal] -> [String] -> IOThrowsError [String]
gen (a : as) acc = gen as [] -- TODO
gen [] result = return result


cg ::
   Env -> 
   LispVal ->
   IOThrowsError [String]
cg _ (Bool False) = return [" PUSH(FALSEOBJ));"]
cg _ (Bool True) = return [" PUSH(TRUEOBJ));"]
-- TODO: (else (list " PUSH(INT2OBJ(" val "));")))))
cg _ e = throwError $ Default $ "Unexpected input to cg: " ++ show e

