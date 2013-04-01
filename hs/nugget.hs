{- | 
Module      : Main
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Portability : portable

An experimental port of the 90 minute scheme compiler to Haskell.
-}

module Main where
import Control.Monad.Error
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Language.Scheme.Core as LSC
import qualified Language.Scheme.Primitives as LSP
import Language.Scheme.Types
import qualified Language.Scheme.Variables as LSV
import System.Environment
import qualified System.Exit
import System.FilePath (dropExtension)
import System.IO
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
    symEnv <- nullEnv
    ast <- loadFile filename
    _ <- run symEnv ast semanticAnalysis 

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

    code <- generateCode env symEnv ast
    putStrLn "-------------------------- C CODE:"
    putStrLn $ show code
    writeOutputFile ((dropExtension filename) ++ ".c")
                    code
    System.Exit.exitSuccess

writeOutputFile :: String -> [String] -> IO ()
writeOutputFile filename code = do
  outH <- liftIO $ openFile filename WriteMode
  writeList outH code
  hClose outH
  
-- |Helper function to write code to file
writeList outH (l : ls) = do
  hPutStrLn outH l
  writeList outH ls
writeList outH _ = do
  hPutStr outH ""

-- TODO: this is a real mess, not general purpose at all
run :: Env -> [LispVal] -> (Env -> LispVal -> IOThrowsError LispVal) -> IO LispVal
run env dat func = do
    result <- runErrorT $ func env $ List dat
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
semanticAnalysis :: Env -> LispVal -> IOThrowsError LispVal
semanticAnalysis env (List (List (Atom "lambda" : List vs : body) : as)) = do
    semanticAnalysis env $ List body
    semanticAnalysis env $ List as
semanticAnalysis env (List (List [Atom "define", Atom var, form] : as)) = do
  --penv <- liftIO $ LSV.printEnv env
  isV <- saLookup env var 
  case isV of
    Bool False -> throwError $ Default $ "cannot set a nonvariable " ++ var -- ++ " - " ++ show isV ++ " - " ++ show penv
    _ -> semanticAnalysis env form
  semanticAnalysis env $ List as
semanticAnalysis env (List (a : as)) = (trace ("a = " ++ show a ) semanticAnalysis) env $ List as
semanticAnalysis env (List []) = return $ Nil ""
semanticAnalysis env _ = return $ Nil ""
--semanticAnalysis env ast = 
--    throwError $ Default $ "SA Unrecognized form: " ++ show ast

-- Port of xe-lookup
saLookup :: Env -> String -> IOThrowsError LispVal
saLookup env var = do
 isV <- liftIO $ isVar env var
 if isV
    then LSV.getVar env var
    else (trace ("DEBUG: adding global: " ++ var) newGlobalVar) env var

-- TODO: consolidate common code w/loadFile func above
generateCode env symEnv ast = do
    result <- runErrorT $ codeGenerate env symEnv ast
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
-- Environments
--
-- TODO: expand this into a separate module
globalNamespace :: Char
globalNamespace = 'g'
localNamespace :: Char
localNamespace = 'l'

initSymEnv :: Env -> IOThrowsError LispVal
initSymEnv symEnv = do
    LSV.defineVar symEnv "seq-num" $ Number 0

newVar :: Env -> String -> IOThrowsError LispVal
newVar symEnv id = do
    Number seqNum <- LSV.getVar symEnv "seq-num"
    _ <- LSV.setVar symEnv "seq-num" $ Number (seqNum + 1)
    LSV.defineNamespacedVar symEnv localNamespace id $ Number (seqNum + 1)

newGlobalVar :: Env -> String -> IOThrowsError LispVal
newGlobalVar symEnv id = do
    LSV.defineNamespacedVar symEnv globalNamespace id $ Atom id

isVar, isGlobalVar :: Env -> String -> IO Bool
isVar symEnv var = do
    found <- LSV.isNamespacedRecBound symEnv localNamespace var
    if found
       then return True
       else isGlobalVar symEnv var
isGlobalVar symEnv var = LSV.isNamespacedRecBound symEnv globalNamespace var


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

codeGenerate :: Env -> Env -> [LispVal] -> IOThrowsError [String]
codeGenerate cgEnv symEnv ast = do
   let globalVars = freeVars $ List ast
-- TODO: ^ need to filter out primitives

   _ <- LSC.evalLisp cgEnv $ List [Atom "load", String "code-gen.scm"]
   _ <- LSC.evalLisp cgEnv $ List [Atom "add-lambda!", List [Atom "quote", List ast]]
   String codePrefix <- LSV.getVar cgEnv "code-prefix"
   String codeSuffix <- LSV.getVar cgEnv "code-suffix"

   code <- (trace ("fv = " ++ show globalVars) compileAllLambdas cgEnv symEnv globalVars)
   return $ [
      "#define NB_GLOBALS " ++ show (length globalVars) ++ "\n" ,
      "#define MAX_STACK 100 \n" , -- could be computed...
      codePrefix] ++ code ++ [codeSuffix]

-- |A port of (compile-all-lambdas) from "90 minutes"
compileAllLambdas ::
    Env -> 
    Env ->
    [LispVal] -> 
    IOThrowsError [String]
compileAllLambdas env symEnv globalVars = do
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

        code <- cg env symEnv body globalVars $ reverse vs
        rest <- compileAllLambdas env symEnv globalVars
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

-- A port of (access-var)
accessVar :: Env -> Env -> String -> [LispVal] -> [LispVal] -> IOThrowsError String
accessVar env symEnv var globalVars stackEnv = do
    -- TODO: WTF is square not bound in jae-test????
    penv <- liftIO $ LSV.printEnv symEnv
    isGV <- liftIO $ isGlobalVar symEnv var
    if (trace ("isGV = " ++ show isGV ++ " " ++ show penv ++ " var = " ++ show var) isGV)
       then do
         let Just i = DL.elemIndex (Atom var) globalVars
         varUID <- LSV.getNamespacedVar symEnv globalNamespace var
         return $ "GLOBAL(" ++ show i ++ "/*" ++ var ++ "." ++ show varUID ++ "*/)"
         -- (list "GLOBAL(" i "/*" (var-uid var) "*/)"))
       else do
         -- TODO: pos-in-list call below is probably broken, replaced w/Haskell below...
         --Number pos <- LSC.evalLisp env $ List [Atom "pos-in-list", Atom var, List stackEnv]
         let Just pos = DL.elemIndex (Atom var) stackEnv
         varUID <- LSV.getNamespacedVar symEnv localNamespace var
         let i = (length stackEnv) - pos - 1 
         return $ "LOCAL(" ++ show i ++ "/*" ++ var ++ "." ++ show varUID ++ "*/)"
         -- (list "LOCAL(" i "/*" (var-uid var) "*/)"))))

cg ::
   Env -> 
   Env -> 
   [LispVal] ->  -- ^ ast
   [LispVal] -> -- ^ globalVars
   [LispVal] -> -- ^ stackEnv
   IOThrowsError [String] -- TODO: String probably makes more sense
cg env symEnv (a : as) globalVars stack = do
    h <- cg' env symEnv a globalVars stack
    t <- cg env symEnv as globalVars stack
    return $ h ++ t
cg env symEnv [] globalVars stack = return []

cg' ::
   Env -> 
   Env -> 
   LispVal ->  -- ^ ast
   [LispVal] -> -- ^ globalVars
   [LispVal] -> -- ^ stackEnv
   IOThrowsError [String] -- TODO: String probably makes more sense
cg' env symEnv (List [Atom "define", Atom var, form]) globalVars stack = do
  h <- cg' env symEnv form globalVars stack
  t <- accessVar env symEnv var globalVars stack
  return $ h ++ [" " ++ t ++ " = TOS();"]
-- this case is impossible after CPS-conversion
cg' env symEnv ast@(List (Atom "lambda" : List vs : body)) globalVars stack = do
   Number i <- LSC.evalLisp env $ 
        List [Atom "add-lambda!", List [Atom "quote", List [ast]]]
   return $ [" PUSH(INT2OBJ(" ++ show i ++ "));"]
cg' env symEnv (List (Atom fnc : args)) globalVars stack = do
    case DM.lookup fnc primitives of
        Just prim -> do
            argStr <- cg env symEnv args globalVars stack
            return $ argStr ++ [prim]
        Nothing -> 
            -- TODO: could be a closure
            throwError $ Default $ "Unknown primitive: " ++ show fnc
cg' _ _ (Bool False) _ _ = return [" PUSH(FALSEOBJ));"]
cg' _ _ (Bool True) _ _ = return [" PUSH(TRUEOBJ));"]
cg' _ _ (Number n) _ _ = return [" PUSH(INT2OBJ(" ++ show n ++ "));"]
cg' _ _ e _ _ = throwError $ Default $ "Unexpected input to cg: " ++ show e

primitives = DM.fromList
  [ ("=", " EQ();")
  , ("<", " LT();")
  , ("+", " ADD();")
  , ("-", " SUB();")
  , ("*", " MUL();")
  , ("display", " DISPLAY();")
  , ("halt", " HALT();")
  ]

