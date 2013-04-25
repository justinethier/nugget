{- | 
Module      : Main
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Portability : portable

nugget, a compiler for a tiny subset of Scheme.

This project is an experimental port of the 90 minute scheme compiler to Haskell.

-}{-------------------------------------------
Terms used in the code:

    * AST - Abstract syntax tree
    * CC - Closure conversion
    * CPS - Continuation-passing style
---------------------------------------------}

module Main where

import Paths_nugget (getDataFileName)
import Control.Monad.Error
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Language.Scheme.Core as LSC
import qualified Language.Scheme.Macro as LSM
import qualified Language.Scheme.Primitives as LSP
import Language.Scheme.Types -- Not worth the effort to qualify
import qualified Language.Scheme.Variables as LSV
import System.Cmd (system)
import System.Console.GetOpt
import System.Environment
import qualified System.Exit
import System.FilePath (dropExtension)
import System.IO
-- import Debug.Trace

main :: IO ()
main = do 
    args <- getArgs

    let (actions, nonOpts, msgs) = getOpt Permute options args
    opts <- foldl (>>=) (return defaultOptions) actions
    let Options {optVerbose = verbose} = opts

    case (null args) of
        True -> showBanner
        _ -> compileFile (head args) verbose

-- Command line options section
data Options = Options {
    optVerbose :: Bool
    }

-- |Default values for the command line options
defaultOptions :: Options
defaultOptions = Options {
    optVerbose = False
    }
options :: [OptDescr (Options -> IO Options)]
options = [
  Option ['v'] ["verbose"] (NoArg getVerbose) "verbose output"
  ]

getVerbose opt = return opt { optVerbose = True }

showBanner :: IO ()
showBanner = putStrLn "Usage: nsc filename"

compileFile :: String -> Bool -> IO ()
compileFile filename verbose = do
    env <- LSC.r5rsEnv -- Local Env for code generation phase
    ast <- loadFile filename

    when verbose (do
        putStrLn "-------------------------- AST:"
        putStrLn $ show ast)

    List astAfterMacroExp <- expandMacro env (List ast)
    when verbose (do
        putStrLn "-------------------------- AST AFTER MACRO EXPANSION:"
        putStrLn $ show astAfterMacroExp)

    -- Analyze expanded ast
    symEnv <- nullEnv
    _ <- initEnv symEnv
    _ <- run symEnv astAfterMacroExp semanticAnalysis 
    -- TODO: output symbol table in verbose mode

    astAfterCPS <- cpsConvert env symEnv astAfterMacroExp
    when verbose (do
        putStrLn "-------------------------- AST AFTER CPS-CONVERSION:"
        putStrLn $ show astAfterCPS)

    astAfterCC <- closureConvert env symEnv astAfterCPS
    when verbose (do
        putStrLn "-------------------------- AST AFTER CLOSURE-CONVERSION:"
        putStrLn $ show astAfterCC)

    (fullcode, c) <- generateCode env symEnv astAfterCC
    when verbose (do
        putStrLn "-------------------------- C CODE:"
        putStr $ joinCode c)

    writeOutputFile ((dropExtension filename) ++ ".c")
                    (joinCode fullcode)

    system $ "gcc " ++ (dropExtension filename) ++ ".c " 
                    ++ "-o " ++ (dropExtension filename)

    System.Exit.exitSuccess

writeOutputFile :: String -> String -> IO ()
writeOutputFile filename code = do
  outH <- liftIO $ openFile filename WriteMode
  hPutStr outH code
  hClose outH
  
-- -- |Helper function to write code to file
-- writeList outH (l : ls) = do
--   hPutStr outH l
--   writeList outH ls
-- writeList outH _ = do
--   hPutStr outH ""

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

expandMacro :: Env -> LispVal -> IO LispVal
expandMacro env ast = do
    result <- runErrorT $ LSM.expand env False ast LSC.apply
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
semanticAnalysis env (List (a : as)) = semanticAnalysis env $ List as
semanticAnalysis env (List []) = return $ Nil ""
semanticAnalysis env _ = return $ Nil ""
--semanticAnalysis env ast = 
--    throwError $ Default $ "SA Unrecognized form: " ++ show ast

-- |A port of xe-lookup
saLookup :: Env -> String -> IOThrowsError LispVal
saLookup env var = do
 isV <- liftIO $ isVar env var
 if isV
    then LSV.getVar env var
    --else (trace ("DEBUG: adding global: " ++ var) newGlobalVar) env var
    else newGlobalVar env var

------------------------------------------------------------------------------
--
-- |Perform CPS (continuation passing style) conversion
--
--  This step also adds the top-level continuation, which calls into 
--  HALT to stop the program.
cpsConvert env symEnv ast = do
  result <- runErrorT $ cpsConvert' env symEnv ast
  handleResult result

cpsConvert' :: Env -> Env -> [LispVal] -> IOThrowsError [LispVal]
cpsConvert' env symEnv ast = do
  -- Top-level is a sequence of expr's, 
  -- so use cpsSeq to convert each one
  _ <- newVar symEnv "r"
  cpsSeq env symEnv
    ast
    (List (Atom "lambda" : List [Atom "r"] : [List [Atom "halt", Atom "r"]]))

cps :: Env -> Env -> LispVal -> LispVal -> IOThrowsError [LispVal] 

-- TODO: cond (really if)

cps env symEnv (List (Atom "lambda" : List vs : body)) (contAST) = do
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
      return 
        [List [contAst,
               List [Atom "define", Atom var, val]]]
-- Application of an anonymous lambda
cps env 
    symEnv 
    (List (List (Atom "lambda" : List vs : body) : as))
    contAst = cpsList env symEnv as innerLamb
  where
    innerLamb e se vals = do
      b <- cpsSeq e se body contAst
      return
        (List (Atom "lambda" : List vs : b) : vals)

-- Function application
cps env symEnv (List ast@(fnc : as)) contAst = do
   case fnc of
     Atom a -> do
       case DM.member a primitives of
         True -> cpsList env symEnv as innerPrim
         _ -> cpsList env symEnv ast innerFunc
     _ -> cpsList env symEnv ast innerFunc
 where 
   innerPrim env symEnv args = do
     return
        [List [contAst,
               List (fnc : args)]]
   innerFunc env symEnv (arg : args) = do
     return 
        [List (arg : contAst : args)]
cps env symEnv ast@(Bool _) contAst = return $ [List [contAst, ast]]
cps env symEnv ast@(Number _) contAst = return $ [List [contAst, ast]]
cps env symEnv ast@(Atom _) contAst = return $ [List [contAst, ast]]
cps _ _ (List []) result = return [result]
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
--
-- |Perform Closure conversion (CC)
closureConvert env symEnv asts = do
  result <- runErrorT $ ccSeq env symEnv asts
  handleResult result

-- |Perform CC on a block of code
ccSeq env symEnv asts = mapM (cc env symEnv (Bool False) []) asts

-- |Perform actual CC
cc :: Env -- ^ Main environment
   -> Env -- ^ Symbol environment
   -> LispVal -- ^ "self" variable
   -> [LispVal] -- ^ Free variables
   -> LispVal -- ^ AST being converted
   -> IOThrowsError LispVal -- ^ Converted AST
cc env symEnv _ _ ast@(Bool _) = return ast 
cc env symEnv _ _ ast@(Number _) = return ast 
cc env symEnv selfVar freeVarLst ast@(List [Atom "define", Atom var, form]) = do
    val <- cc env symEnv selfVar freeVarLst form
    return $ List [Atom "define", Atom var, val]
cc env symEnv selfVar freeVarLst ast@(Atom a) = do
  case DL.elemIndex ast freeVarLst of
    Just i -> do
        return $ List [Atom "%closure-ref", selfVar, Number $ toInteger (i + 1)]
    Nothing -> return ast

-- TODO: cnd (if)  

-- Lambda
cc env symEnv selfVar freeVarLst 
   ast@(List (Atom "lambda" : List vs : body)) = do
  fv <- freeVars symEnv ast
  let filterFV (Atom v) = do
        is <- isGlobalVar symEnv v
        return $ not is 
  newFreeVars <- liftIO $ filterM filterFV fv
--  penv <- (trace ("ast = " ++ show ast) liftIO) $ LSV.printEnv symEnv -- DEBUG!
--  _ <- (trace ("fv = " ++ show fv ++ ", new free vars = " ++ show newFreeVars ++ ", symEnv = " ++ show penv) newVar) symEnv "self"
  _ <- newVar symEnv "self"

  bodyConv <- mapM (cc env symEnv (Atom "self") newFreeVars) body
  let l = List (Atom "lambda" : List (Atom "self" : vs) : bodyConv)
  v <- mapM (\ v -> cc env symEnv selfVar freeVarLst v) newFreeVars
  return $ List (Atom "%closure" : l : v)

-- |Function application
cc env symEnv selfVar freeVarLst ast@(List (Atom fnc : args)) = do
  args' <- mapM (\ v -> cc env symEnv selfVar freeVarLst v)
                args 

  case DM.member fnc primitives of
    True -> do
      return $ List (Atom fnc : args')
    False -> do
      f <- cc env symEnv selfVar freeVarLst (Atom fnc)
      return $ 
        List (List [Atom "%closure-ref", f, Number 0] : f : args')

-- Lambda application
cc env symEnv selfVar freeVarLst 
   (List (fnc@(List (Atom "lambda" : List vs : body)) : 
          args)) = do
  args' <- mapM (\ v -> cc env symEnv selfVar freeVarLst v) args 
  body' <- mapM (\ v -> cc env symEnv selfVar freeVarLst v) body
  return $ List (List (Atom "lambda" : List vs : body') :
                 args')

cc env symEnv _ _ ast = 
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
--
-- TODO: is this good enough, or do we need more context information, since
--       a passed arg could have the same name as a primitive
-- the thing is, 90-scc has an AST that includes context information and
-- can distinguish between an Atom that is a ref versus one that is
-- used as a prim.
--
freeVars :: 
    Env -> 
    LispVal -> 
    IOThrowsError [LispVal] 
freeVars symEnv ast = do
    -- Filter out primitives
    let fv = freeVars' ast
        prims = map (\ v -> Atom v) 
                    (DM.keys primitives)
        sym = DS.toList $ DS.difference (DS.fromList fv)
                                        (DS.fromList prims)
    return sym
--
-- The below does not work because it filters out
-- lambda parameters that can still be free variables even though
-- they are not allocated using newVar
--
--  -- TODO: experimenting with filtering out symbols that are
--  -- not variables, instead of just returning all found symbols
--  -- I think we still have a context problem, though
--        filterFV (Atom v) = do
--          isL <- isVar symEnv v
--          isG <- isGlobalVar symEnv v
--          return $ isL || isG
--    liftIO $ filterM filterFV sym

freeVars' v@(Atom _) = [v]
freeVars' (List (Atom "define" : v@(Atom _) : rest)) = do
    DS.toList $ DS.union (DS.fromList [v]) 
                         (DS.fromList (freeVars' $ List rest))
freeVars' (List (Atom "lambda" : List vs : body)) =
    DS.toList $ DS.difference (DS.fromList (freeVars' $ List body))
                              (DS.fromList vs)
freeVars' (List ast@(fnc : _)) = do
    let fvs = map (\ l -> DS.fromList $ freeVars' l) ast
    DS.toList $ DS.unions fvs
freeVars' _ = []

---------------------------------------------------------------------
--
-- |Generate the final C code
joinCode [] = []
joinCode (l:ls) = l ++ joinCode ls

generateCode env symEnv ast = do
    result <- runErrorT $ codeGenerate env symEnv ast
    handleResult result

codeGenerate :: Env -> Env -> [LispVal] -> IOThrowsError ([String], [String])
codeGenerate cgEnv symEnv ast = do
   globalVars <- freeVars symEnv $ List ast

   lib <- liftIO $ getDataFileName "lib/nugget.scm"
   _ <- LSC.evalLisp cgEnv $ List [Atom "load", String lib]
   _ <- LSC.evalLisp cgEnv $ List [Atom "add-lambda!", List [Atom "quote", List (Atom "lambda" : List [] : ast)]]
   String codePrefix <- LSV.getVar cgEnv "code-prefix"
   String codeSuffix <- LSV.getVar cgEnv "code-suffix"

   --code <- (trace ("fv = " ++ show globalVars) compileAllLambdas cgEnv symEnv globalVars)
   code <- compileAllLambdas cgEnv symEnv globalVars
   return ([
      "#define NB_GLOBALS " ++ show (length globalVars) ++ "\n" ,
      "#define MAX_STACK 100 \n" , -- could be computed...
      codePrefix] ++ code ++ [codeSuffix], code)

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
        --case (trace ("\nDEBUG: todo = " ++ show todo ++ "\nast = " ++ show ast ++ "\n") ast) of
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
    penv <- liftIO $ LSV.printEnv symEnv
    isGV <- liftIO $ isGlobalVar symEnv var
    --if (trace ("isGV = " ++ show isGV ++ " " ++ show penv ++ " var = " ++ show var ++ " stack = " ++ show stackEnv) isGV)
    if isGV
       then do
         Number i <- case DL.elemIndex (Atom var) globalVars of
           Just i -> return $ Number $ toInteger i
           Nothing -> throwError $ Default $ "accessVar: global not found for variable " ++ var
         varUID <- LSV.getNamespacedVar symEnv globalNamespace var
         return $ "GLOBAL(" ++ show i ++ "/*" ++ var ++ "." ++ show varUID ++ "*/)"
       else do
         let position = DL.elemIndex (Atom var) stackEnv
         case position of
           Just pos -> do
             --varUID <- LSV.getNamespacedVar symEnv localNamespace var
             let i = (length stackEnv) - pos - 1 
             return $ "LOCAL(" ++ show i ++ "/*" ++ var ++ {-"." ++ show varUID ++-} "*/)"
           Nothing -> throwError $ Default $
             "Unable to access local variable " ++ var

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

-- |Uses cgList to generate code for function arguments
-- TODO: or at least, I think so. need to analyse both funcs more
cgArgs :: Env -> Env -> [LispVal] -> [LispVal] -> [LispVal] -> IOThrowsError [String]
cgArgs env symEnv args globalVars stackEnv = do
    let vars = map (\ n -> Number $ toInteger n) (interval 1 $ length args)
        cont _ _ code _ = return code
    cgList env symEnv args vars globalVars stackEnv "" cont

-- |Generate code for a list of AST forms
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

-- |Generate C code for an AST
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
    throwError $ Default $
        "Code generation encountered unexpected lambda: " ++ show ast
--   Number i <- LSC.evalLisp env $ 
--        List [Atom "add-lambda!", List [Atom "quote", List [ast]]]
--   (trace "should be impossible!!!!!" return) $ [" PUSH(INT2OBJ(" ++ show i ++ "));"]

-- |Application of an anonymous lambda
cg' env symEnv (List (lam@(List (Atom "lambda" : List vs : body)) : args))
    globalVars stack = do
    cgList env symEnv args vs globalVars stack "\n" cont
  where cont e se code newStack = do
          rest <- cg e se body globalVars newStack
          return $ code ++ rest

cg' env symEnv (List (Atom "%closure" : args)) globalVars stack = do
   Number i <- LSC.evalLisp env $ 
        List [Atom "add-lambda!", List [Atom "quote", head args]]
   let n = length $ tail args
       s = ["CLOSURE(" ++ show i ++ "," ++ show n ++ ");"]
   code <- cgArgs env symEnv (tail args) globalVars stack

   return $ code ++ 
            (" BEGIN_" : s) ++ 
            (map (\ j -> " INICLO(" ++ show j ++ ");") 
                 (reverse $ interval 1 n)) ++
            (" END_" : s)

cg' env symEnv (List (Atom "%closure-ref" : args)) globalVars stack = do
    let i = head $ tail args
    code <- cg' env symEnv (head args) globalVars stack

    return $ code ++
             [" TOS() = CLOSURE_REF(TOS()," ++ show i ++ ");"]

-- |Function application
cg' env symEnv (List (fnc : args)) globalVars stack = do
    case fnc of
        Atom f -> case DM.lookup f primitives of
            Just prim -> do
                -- Primitive function application
                argStr <- cg env symEnv args globalVars stack
                return $ argStr ++ [prim]
            Nothing -> genApp
        _ -> genApp
  where 
    -- General function application
    genApp = do
      code <- cg env symEnv args globalVars stack
      let n = length args
          start = length stack
          s = "JUMP(" ++ show n ++ ");"
          frame = map (\ j -> " PUSH(LOCAL(" ++ show (j + start) ++ "));") 
                      (interval 0 (n - 1))
      return $ code 
               ++ [" BEGIN_" ++ s]
               ++ frame
               ++ [" END_" ++ s]

cg' _ _ (Bool False) _ _ = return [" PUSH(FALSEOBJ));"]
cg' _ _ (Bool True) _ _ = return [" PUSH(TRUEOBJ));"]
cg' _ _ (Number n) _ _ = return [" PUSH(INT2OBJ(" ++ show n ++ "));"]
cg' env symEnv (Atom var) globalVars stack = do
    code <- accessVar env symEnv var globalVars stack
    return [" PUSH(" ++ code ++ ");"]
cg' _ _ e _ _ = throwError $ Default $ "Unexpected input to cg: " ++ show e

-- |Primitive functions implemented directly in C
primitives = DM.fromList
  [ ("=", " EQ();")
  , ("<", " LT();")
  , ("+", " ADD();")
  , ("-", " SUB();")
  , ("*", " MUL();")
  , ("display", " DISPLAY();")
  , ("halt", " HALT();")
  ]

