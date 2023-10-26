module Llvm where 

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad
import System.Process
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified AbsInstant as I
import qualified Data.Text as T

type StateLlvm = (Set.Set String, Int) -- keeping used variables

incSt :: StateLlvm -> StateLlvm
incSt (s, i) = (s, i + 1)

declareVar :: String -> StateLlvm -> StateLlvm
declareVar tag (s, i) = (Set.insert tag s, i)

checkIfDeclared :: String -> StateLlvm -> Bool
checkIfDeclared tag (s, i) = Set.member tag s

type InstantLlvmMonad a = StateT StateLlvm (Writer String) a
runMonad :: StateLlvm -> InstantLlvmMonad a -> String
runMonad st env = snd $ runWriter (runStateT env st)

data Value 
    = Tag String 
    | VBlank 
    deriving (Eq, Show, Ord)

--- CONSTANTS ---
cADD_LLVM_COMMAND = "add"
cSUB_LLVM_COMMAND = "sub"
cMUL_LLVM_COMMAND = "mul"
cDIV_LLVM_COMMAND = "sdiv"
cLLVM_START = "declare void @printInt(i32)\ndefine i32 @main()\n{\n"
cLLVM_END = "\tret i32 0\n}\n"

--- AUXILIARY ---
allocLineStr :: String -> String
allocLineStr tag = "\t%" ++ tag ++ " = alloca i32\n"

storeLineStr :: String -> String -> String
storeLineStr tag1 tag2 = "\tstore i32 %" ++ tag1 ++ ", i32* %" ++ tag2 ++ "\n"

loadLineStr :: String -> String -> String 
loadLineStr ind tag = "\t%" ++ ind ++ " = load i32, i32* %" ++ tag ++ "\n";

printLineStr :: String -> String 
printLineStr tag = "\tcall void @printInt(i32 %" ++ tag ++ ")\n"

operationLineStr :: String -> String -> String -> String -> String 
operationLineStr ind op tag1 tag2 = "\t%" ++ ind ++ " = " ++ op ++ " i32 %" ++ tag1 ++ ", %" ++ tag2 ++ "\n";

--- EXPRESSIONS ---
evalExp :: I.Exp -> InstantLlvmMonad Value

evalExp (I.ExpAdd e1 e2) = evalExpBinOp e1 e2 cADD_LLVM_COMMAND

evalExp (I.ExpSub e1 e2) = evalExpBinOp e1 e2 cSUB_LLVM_COMMAND

evalExp (I.ExpMul e1 e2) = evalExpBinOp e1 e2 cMUL_LLVM_COMMAND

evalExp (I.ExpDiv e1 e2) = evalExpBinOp e1 e2 cDIV_LLVM_COMMAND

evalExp (I.ExpLit val) = return (Tag ("#" ++ show val)) -- Marking the literal value

evalExp (I.ExpVar (I.Ident tag)) = do
    modify incSt
    st <- get
    let indStr = show $ snd st
    tell $ loadLineStr indStr tag
    return (Tag indStr)

evalExpBinOp :: I.Exp -> I.Exp -> String -> InstantLlvmMonad Value
evalExpBinOp e1 e2 op = do 
    val1 <- evalExp e1 
    val2 <- evalExp e2 
    modify incSt
    case (val1, val2) of 
        (Tag tag1, Tag tag2) -> do 
            st <- get
            let indStr = show $ snd st
            tell $ operationLineStr indStr op tag1 tag2
            return (Tag indStr)

        _ -> error "Not possible to get there!"

--- STATEMENTS ---
evalStmt :: [I.Stmt] -> InstantLlvmMonad Value

evalStmt a@(I.SAss (I.Ident newtag) exp : tstmts) = do
    val <- evalExp exp
    case val of
        Tag tag -> do 
            st <- get 
            let isDeclared = checkIfDeclared newtag st

            if isDeclared then do
                tell $ storeLineStr tag newtag
                evalStmt tstmts
            else do 
                tell $ allocLineStr newtag
                modify (declareVar newtag)
                tell $ storeLineStr tag newtag
                evalStmt tstmts

        _ -> error "Not possible to get there!"

evalStmt a@((I.SExp exp) : tstmts) = do
    val <- evalExp exp
    case val of 
        Tag tag -> do 
            tell $ printLineStr tag
            evalStmt tstmts

        _ -> error "Not possible to get there!" 

evalStmt [] = return VBlank

runInstant :: I.Program -> String -> IO ()
runInstant (I.Prog stmts) filepath = do
    let st = (Set.fromList [], 0)
    let resultRaw = runMonad st (evalStmt stmts) -- Putting constant values
    let result = concatMap T.unpack (T.splitOn (T.pack "%#") (T.pack resultRaw))
    writeFile (filepath ++ ".ll") (cLLVM_START ++ result ++ cLLVM_END)

    _ <- system $ "llvm-as -o temporary.bc " ++ filepath ++ ".ll"
    _ <- system $ "llvm-link -o " ++ filepath ++ ".bc temporary.bc lib/runtime.bc"
    _ <- system $ "rm temporary.bc"
    
    return ()