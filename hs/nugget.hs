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
    _ <- initEnv symEnv
    _ <- run symEnv ast semanticAnalysis 

    putStrLn "-------------------------- AST:"
    putStrLn $ show ast

    astAfterCPS <- cpsConvert env symEnv ast
    putStrLn "-------------------------- AST AFTER CPS-CONVERSION:"
    putStrLn $ show astAfterCPS

    astAfterCC <- closureConvert env symEnv astAfterCPS
    putStrLn "-------------------------- AST AFTER CLOSURE-CONVERSION:"
    putStrLn $ show astAfterCC

    code <- generateCode env symEnv astAfterCC
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
  hPutStr outH l
  writeList outH ls
writeList outH _ = do
  hPutStr outH ""

handleResult (Left err) = do
  putStrLn $ show err
  System.Exit.exitFailure
handleResult (Right val) = return val

-- TODO: this is a real mess, not general purpose at all
run :: Env -> [LispVal] -> (Env -> LispVal -> IOThrowsError LispVal) -> IO LispVal
run env dat func = do
    result <- runErrorT $ func env $ List dat 
    handleResult result

loadFile :: String -> IO [LispVal]
loadFile filename = do
    result <- runErrorT $ LSP.load filename
    handleResult result

initEnv symEnv = do
    result <- runErrorT $ initSymEnv symEnv
    handleResult result

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

generateCode env symEnv ast = do
    result <- runErrorT $ codeGenerate env symEnv ast
    handleResult result

------------------------------------------------------------------------------
-- CPS (continuation passing style) conversion
cpsConvert env symEnv ast = do
  result <- runErrorT $ cpsConvert' env symEnv ast
  handleResult result
cpsConvert' :: Env -> Env -> [LispVal] -> IOThrowsError [LispVal]
cpsConvert' env symEnv ast = do
  _ <- newVar symEnv "r"
  cpsSeq env symEnv  -- Top-level is a sequence of expr's, so use cpsSeq
    ast
    (List (Atom "lambda" : List [Atom "r"] : [List [Atom "halt", Atom "r"]]))

cps :: Env -> Env -> LispVal -> LispVal -> IOThrowsError [LispVal] 
cps env symEnv (List (Atom "lambda" : List vs : body)) (contAST) = do
-- OBSOLETE - the old code for above - cps env symEnv (List (List (Atom "lambda" : List vs : body) : as)) (contAST) = do
    _ <- newVar symEnv "k" -- TODO: will this clobber an old k?
                        -- may need to rethink var type, may even need to
                        -- use a new data type that can pass along metadata
                        -- such as var, id, uid, etc
    b <- cpsSeq env symEnv body $ Atom "k"
    return $ [List [contAST, 
                   (List (Atom "lambda" : List (Atom "k" : vs) : b))]]
cps env symEnv (List [Atom "define", Atom var, form]) contAst = do
    cpsList env symEnv [form] inner
  where 
    inner env symEnv [val] = do
      --throwError $ Default $ "val = " ++ show val
      (trace ("val = " ++ show val) return) [List [contAst,
                     List [Atom "define", Atom var, val]]]

-- TODO: prim
-- TODO: cond (really if)
-- TODO: function app (will cause problems with below, which is really the seq case)
--
-- maybe need to move this to process lambda body, since that is the only
-- place that can have a sequence anyway, except the top-level which is
-- a special case
--
-- so we then need to change this to accept either an anonymous function
-- (lambda), primitive function application, or general function application.
-- 
-- TBD: probably should search the primitives list to determine if prim, 
-- structure so it is a good foundation to extend later
-- 
cps env symEnv (List ast@(Atom a : as)) contAst = do
   case DM.member a (trace ("app, ast = " ++ show ast) primitives) of
        True -> cpsList env symEnv as innerPrim
        _ -> cpsList env symEnv as innerFunc
   
 where 
   innerPrim env symEnv args = do
     (trace ("innerPrim, args = " ++ show args) return)
     --return
        [List [contAst,
               List (Atom a : args)]]
   innerFunc env symEnv args = do
     (trace ("innerFunc, args = " ++ show args) return)
     --return 
        [List (Atom a : contAst : args)]
-- TODO: application of an anonymous lambda
--cps env symEnv (List (??? : as)) contAst = do
    


cps _ _ (List []) result = return [result]
cps env symEnv ast@(Bool _) contAst = return $ [List [contAst, ast]]
cps env symEnv ast@(Number _) contAst = return $ [List [contAst, ast]]
cps env symEnv ast@(Atom _) contAst = return $ [List [contAst, ast]]
cps _ _ err contAst = throwError $ Default $ "Unexpected input to cps, ast = " ++ (show err) ++ ", cont ast = " ++ show contAst

-- |Convert a list (sequence) of code into CPS
cpsSeq env symEnv [] contAst = return [List [contAst, Bool False]]
cpsSeq env symEnv [ast] contAst = cps env symEnv ast contAst
cpsSeq env symEnv (a : as) contAst = do
    _ <- newVar symEnv "r"
    b <- cpsSeq env symEnv as contAst
    cps env symEnv a (List (Atom "lambda" : List [Atom "r"] : b)) 

--TODO: this is a port of cps-list, but what exactly does it do??
cpsList :: Env -> Env -> [LispVal] ->
          (Env -> Env -> [LispVal] -> IOThrowsError [LispVal]) ->
           IOThrowsError [LispVal]
cpsList env symEnv [] inner = inner env symEnv []
cpsList env symEnv (Atom a : as) inner = cpsListBody env symEnv (Atom a : as) inner
cpsList env symEnv (Number a : as) inner = cpsListBody env symEnv (Number a : as) inner
cpsList env symEnv (Bool a : as) inner = cpsListBody env symEnv (Bool a : as) inner
cpsList env symEnv (a : as) inner = do
    _ <- newVar symEnv "r"
    b <- cpsListBody env symEnv (Atom "r" : as) inner
    cps env symEnv a (List (Atom "lambda" : List [Atom "r"] : b))

cpsListBody :: Env -> Env -> [LispVal] ->
              (Env -> Env -> [LispVal] -> IOThrowsError [LispVal]) ->
              IOThrowsError [LispVal]
cpsListBody env symEnv (a : as) inner = do
    cpsList env symEnv as (\ e se newAsts -> inner e se (a : newAsts))

------------------------------------------------------------------------------
-- Closure conversion
closureConvert env symEnv asts = do
  result <- runErrorT $ ccSeq env symEnv asts
  handleResult result

ccSeq env symEnv asts = mapM (cc env symEnv) asts

cc :: Env -> Env -> LispVal -> IOThrowsError LispVal
cc env symEnv ast@(List [Atom "define", Atom var, form]) = do
    val <- cc env symEnv form
    return $ List [Atom "define", Atom var, val]
cc env symEnv ast@(List (Atom fnc : args)) = return ast  -- TODO: app, prim cases
cc env symEnv ast@(Atom _) = return ast  -- TODO: ref case?
-- TODO: lambda case
cc env symEnv ast@(Bool _) = return ast 
cc env symEnv ast@(Number _) = return ast 
cc env symEnv ast = 
  throwError $ Default $ "Unrecognized ast in closure conversion: " ++ show ast

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
-- utilities
interval :: Int -> Int -> [Int]
interval n m 
    | n <= m = (n : interval (n + 1) m)
    | otherwise = []

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
   _ <- LSC.evalLisp cgEnv $ List [Atom "add-lambda!", List [Atom "quote", List (Atom "lambda" : List [] : ast)]]
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

        ast <- LSP.cdr [x]
        case ast of
          -- TODO: is ast always a lambda here? what if it is something else?
          (List (Atom "lambda" : List vs : body)) -> do
            LSP.cdr [todo] >>= LSV.setVar env "lambda-todo" 
            code <- cg env symEnv body globalVars $ reverse vs
            rest <- compileAllLambdas env symEnv globalVars
            return $
                ["case " ++ show caseNum ++ ": /* " ++ show ast ++ " */\n\n"] -- " (object->string (source ast) 60) " */\n\n"
                ++ code ++ ["\n\n"] ++ rest
          err -> throwError $ Default $ "Unexpected pattern in compileAllLambdas: " ++ show err

-- A port of (access-var)
accessVar :: Env -> Env -> String -> [LispVal] -> [LispVal] -> IOThrowsError String
accessVar env symEnv var globalVars stackEnv = do
    -- TODO: WTF is square not bound in jae-test????
    penv <- liftIO $ LSV.printEnv symEnv
    isGV <- liftIO $ isGlobalVar symEnv var
    if (trace ("isGV = " ++ show isGV ++ " " ++ show penv ++ " var = " ++ show var ++ " stack = " ++ show stackEnv) isGV)
       then do
         let Just i = DL.elemIndex (Atom var) globalVars
         varUID <- LSV.getNamespacedVar symEnv globalNamespace var
         return $ "GLOBAL(" ++ show i ++ "/*" ++ var ++ "." ++ show varUID ++ "*/)"
         -- (list "GLOBAL(" i "/*" (var-uid var) "*/)"))
       else do
         -- TODO: pos-in-list call below is probably broken, replaced w/Haskell below...
         --Number pos <- LSC.evalLisp env $ List [Atom "pos-in-list", Atom var, List stackEnv]
         let Just pos = DL.elemIndex (Atom var) stackEnv
         --varUID <- LSV.getNamespacedVar symEnv localNamespace var
         let i = (length stackEnv) - pos - 1 
         return $ "LOCAL(" ++ show i ++ "/*" ++ var ++ {-"." ++ show varUID ++-} "*/)"
         -- (list "LOCAL(" i "/*" (var-uid var) "*/)"))))


-- A port of cg-list
-- TODO: which is used to...?
cgList :: 
    Env -> 
    Env ->
    [LispVal] -> -- ^ AST's
    [LispVal] -> -- ^ Vars 
    [LispVal] -> -- ^ globals
    [LispVal] -> -- ^ stack
    String -> -- ^ separators
    (Env -> Env -> [String] -> [LispVal] -> IOThrowsError [String]) -> -- ^ Continuation
    IOThrowsError [String]
cgList env symEnv [] _ globalVars stackEnv sep cont = do
    cont env symEnv [""] stackEnv 

cgList env symEnv (ast : as) (var : vs) globalVars stackEnv sep cont = do
    x <- cg' env symEnv ast globalVars stackEnv
    cgList env symEnv as vs globalVars (var : stackEnv) sep $
        (\ e se code stack -> do 
            cont e se (x ++ [sep] ++ code) stack)

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
----TODO: for some reason we are getting in here even though it is supposed
----to be impossible after CPS conversion. have a look at the TODO's in 
----the debug output:
------ NOTE: above should be fixed after adding cpsList and *using* it
cg' env symEnv ast@(List (Atom "lambda" : List vs : body)) globalVars stack = do
   Number i <- LSC.evalLisp env $ 
        List [Atom "add-lambda!", List [Atom "quote", List [ast]]]
   return $ [" PUSH(INT2OBJ(" ++ show i ++ "));"]
-- Application of an anonymous lambda
cg' env symEnv (List (lam@(List (Atom "lambda" : List vs : body)) : args)) globalVars stack = do
    --code <- cg env symEnv args globalVars stack
    --lambdaCG <- cg' env symEnv lam globalVars stack
    --return $ code ++ lambdaCG
    cgList env symEnv args vs globalVars stack "\n" cont
--           (\ e se code newStack -> do
  where cont e se code newStack = do
          rest <- cg e se body globalVars newStack
          return $ code ++ rest
-- Above is a port of the following:
-- (cg-list args
--          (lam-params fn)
--          stack-env
--          "\n"
--          (lambda (code new-stack-env)
--            (list
--             code
--             (code-gen (car (ast-subx fn))
--                       new-stack-env))))

cg' env symEnv (List (Atom fnc : args)) globalVars stack = do
    case DM.lookup fnc primitives of
        Just prim -> do
            argStr <- cg env symEnv args globalVars stack
            return $ argStr ++ [prim]
        Nothing -> do
            -- TODO: could be a closure
            --throwError $ Default $ "Unknown primitive: " ++ show fnc

-- TODO: this is a WIP
            -- the app / not lam? case
            code <- cg env symEnv args globalVars stack
            let n = length args
                start = length stack
                s = "JUMP(" ++ show n ++ ");"
                frame = map (\ j -> " PUSH(LOCAL(" ++ show (j + start) ++ "));") $ interval 0 (n - 1)
            return $ code 
                     ++ [" BEGIN_" ++ s]
                     ++ frame
                     ++ [" END_" ++ s]
-- Above is a port of the following:
--                   (cg-list args
--                            (interval 1 n)
--                            stack-env
--                            "\n"
--                            (lambda (code new-stack-env)
--                              (let* ((start (length stack-env))
--                                     (s (list "JUMP(" n ");")))
--                              (list
--                               code
--                               " BEGIN_" s
--                               (map (lambda (j)
--                                      (list " PUSH(LOCAL(" (+ j start) "));"))
--                                    (interval 0 (- n 1)))
--                               " END_" s)))))))

cg' _ _ (Bool False) _ _ = return [" PUSH(FALSEOBJ));"]
cg' _ _ (Bool True) _ _ = return [" PUSH(TRUEOBJ));"]
cg' _ _ (Number n) _ _ = return [" PUSH(INT2OBJ(" ++ show n ++ "));"]
cg' env symEnv (Atom var) globalVars stack = do
    code <- accessVar env symEnv var globalVars stack
    return [" PUSH(" ++ code ++ ");"]
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

