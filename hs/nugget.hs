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
    env <- LSC.r5rsEnv -- Local Env for code generation phase
    ast <- loadFile filename
    _ <- run env ast semanticAnalysis 

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

    code <- generateCode env ast
    putStrLn "-------------------------- C CODE:"
    putStrLn $ show code
    System.Exit.exitSuccess

run :: Env -> [LispVal] -> (Env -> [LispVal] -> IOThrowsError [LispVal]) -> IO [LispVal]
run env dat func = do
    result <- runErrorT $ func env dat
    case result of
        Left err -> do
            putStrLn $ show err
            System.Exit.exitFailure
        Right val -> do
            return val

loadFile :: String -> IO [LispVal]
loadFile filename = do
    result <- runErrorT $ LSP.load filename
    case result of
        Left err -> do
            putStrLn $ show err
            System.Exit.exitFailure
        Right ast -> do
            return $ ast

-- |Build symbol table (IE, GLOBAL / LOCAL), possibly
--  expand macros at this phase
--
-- This would handle functions that "90" performs in the
-- xe (expand expression) phase
semanticAnalysis :: Env -> [LispVal] -> IOThrowsError [LispVal]
semanticAnalysis env ast = do
    --_ <- LSV.defineVar env "TEST" $ String "TESTING"
    return [Nil "TODO"]

-- TODO: consolidate common code w/loadFile func above
generateCode env ast = do
    result <- runErrorT $ codeGenerate env ast
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

codeGenerate :: Env -> [LispVal] -> IOThrowsError [String]
codeGenerate cgEnv ast = do
   let fv = freeVars $ List ast

   _ <- LSC.evalLisp cgEnv $ List [Atom "load", String "code-gen.scm"]
   _ <- LSC.evalLisp cgEnv $ List [Atom "add-lambda!", List [Atom "quote", List ast]]
   String codeSuffix <- LSV.getVar cgEnv "code-suffix"

   code <- (trace ("fv = " ++ show fv) compileAllLambdas cgEnv)
   return $ [
      "#define NB_GLOBALS \n" , -- TODO: (length global-vars) "\n"
      "#define MAX_STACK 100 \n" ] -- could be computed...
      --TODO: codePrefix] 
      ++ code ++ [codeSuffix]

-- |A port of (compile-all-lambdas) from "90 minutes"
compileAllLambdas ::
    Env -> 
    IOThrowsError [String]
compileAllLambdas env = do
    todo <- LSV.getVar env "lambda-todo"
    isTodo <- LSP.isNull [todo]
    case isTodo of
       Bool True -> return [] 
       _ -> do
        x <- LSP.car [todo]
        caseNum <- LSP.car [x]
        -- TODO: is ast always a lambda here? what if it is something else?
        ast@(List [List (Atom "lambda" : List vs : body)]) <- LSP.cdr [x]
        LSP.cdr [todo] >>= LSV.setVar env "lambda-todo" 
       
--test <- LSV.getVar env "TEST"
        -- TODO: how to differentiate ast and ast-subx???
        --astH <- LSP.car [ast]
        --astT <- LSP.cdr [ast]

        code <- cg env (List body) $ reverse vs
        rest <- compileAllLambdas env
        return $
            ["case " ++ show caseNum ++ ": /* " ++ show ast ++ " */\n\n"] -- " (object->string (source ast) 60) " */\n\n"
            ++ code ++ ["\n\n"] ++ rest

-- TODO: a port of cg-list
--cgList :: 
--    Env -> 

--    (define (cg-list asts vars stack-env sep cont)
--      (if (null? asts)
--          (cont "" stack-env)
--          (let ((x (code-gen (car asts) stack-env)))
--            (cg-list (cdr asts)
--                     (cdr vars)
--                     (cons (car vars) stack-env)
--                     sep
--                     (lambda (code stack-env)
--                       (cont (list x sep code)
--                             stack-env))))))

cg ::
   Env -> 
   LispVal ->  -- ^ ast
   [LispVal] -> -- ^ stackEnv
   IOThrowsError [String]
cg _ (Bool False) _ = return [" PUSH(FALSEOBJ));"]
cg _ (Bool True) _ = return [" PUSH(TRUEOBJ));"]
-- TODO: (else (list " PUSH(INT2OBJ(" val "));")))))
cg _ e _ = throwError $ Default $ "Unexpected input to cg: " ++ show e

