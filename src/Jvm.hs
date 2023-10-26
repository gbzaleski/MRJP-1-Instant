module Jvm where 

import Control.Monad.Writer
import Control.Monad.State
import System.Process
import System.FilePath (takeBaseName, replaceExtension, takeDirectory)
import Data.Text.Lazy.Builder
import qualified Data.Text.Lazy as L 
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import qualified AbsInstant as I

data StateJvm = St { -- 
  variables :: M.Map String Int, -- Matches variables to their indices
  stackSize :: Int, 
  localSize :: Int }
    deriving Show

declareVar :: String -> Int -> StateJvm -> StateJvm
declareVar tag ind st = st {variables = M.insert tag ind (variables st)}

getVariableByTag :: String -> StateJvm -> Int
getVariableByTag tag st = (variables st) M.! tag

updateStackSize :: Int -> StateJvm -> StateJvm
updateStackSize newStackSize st = st {stackSize = max newStackSize (stackSize st)}

updateLocalSize :: Int -> StateJvm -> StateJvm
updateLocalSize newLocalSize st = st {localSize = max newLocalSize (localSize st)}

incLocalSize :: StateJvm -> StateJvm
incLocalSize st = st {localSize = (localSize st) + 1}

data Value 
    = Node Builder Int -- Value & Depth (for balancing), Lazy.Builder for optimal string construction
    | VBlank 
    deriving (Eq, Show, Ord)

type InstantJvmMonad a = StateT StateJvm (Writer String) a
runMonad :: StateJvm -> InstantJvmMonad a -> String
runMonad st env = snd $ runWriter (runStateT env st)

--- CONSTANTS ---
cADD_JVM_COMMAND = "iadd"
cSUB_JVM_COMMAND = "isub"
cMUL_JVM_COMMAND = "imul"
cDIV_JVM_COMMAND = "idiv"
cPRINT_COMMAND = "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n\tswap\n\tinvokevirtual java/io/PrintStream.println(I)V\n"

cICONST_MAX = 4
cBIPUSH_MAX = 255
cSIPUSH_MAX = 65535

--- AUXILIARY ---
getIntType :: Integer -> String 
getIntType val = 
    if val == -1 then "iconst_m1"
    else if val <= cICONST_MAX then "iconst_" ++ show val
    else if val <= cBIPUSH_MAX then "bipush " ++ show val
    else if val <= cSIPUSH_MAX then "sipush " ++ show val
    else "ldc " ++ show val

loadLineStr :: Int -> String
loadLineStr val = "\tiload" ++ (if val <= 3 then "_" else " ") ++ show val ++ "\n"

storeLineStr :: Int -> String
storeLineStr val = "\tistore" ++ (if val <= 3 then "_" else " ") ++ show val ++ "\n"

generateJvmFile :: String -> String -> String -> String -> String
generateJvmFile result className localSize stackSize = 
    ".class public " ++ className ++ "\n" ++ 
    ".super java/lang/Object\n\n" ++
    ".method public <init>()V\n" ++
    "\taload_0\n" ++
    "\tinvokespecial java/lang/Object/<init>()V\n" ++
    "\treturn\n" ++
    ".end method\n\n" ++
    ".method public static main([Ljava/lang/String;)V\n" ++
    "\t.limit locals " ++ localSize ++ "\n" ++
    "\t.limit stack " ++ stackSize ++ "\n" ++
    result ++
    "\treturn\n" ++ 
    ".end method\n"

--- EXPRESSIONS ---
evalExp :: I.Exp -> InstantJvmMonad Value

evalExp (I.ExpAdd e1 e2) = evalExpBinOp e1 e2 cADD_JVM_COMMAND True 

evalExp (I.ExpSub e1 e2) = evalExpBinOp e1 e2 cSUB_JVM_COMMAND False

evalExp (I.ExpMul e1 e2) = evalExpBinOp e1 e2 cMUL_JVM_COMMAND True

evalExp (I.ExpDiv e1 e2) = evalExpBinOp e1 e2 cDIV_JVM_COMMAND False

evalExp (I.ExpLit val) = return $ Node (fromString ("\t" ++ getIntType val ++ "\n")) 1 

evalExp (I.ExpVar (I.Ident tag)) = do 
    st <- get
    let ind = getVariableByTag tag st -- It is assumed only declared variables are used
    return $ Node (fromString (loadLineStr ind)) 1

evalExpBinOp :: I.Exp -> I.Exp -> String -> Bool -> InstantJvmMonad Value
evalExpBinOp e1 e2 op isCommutative = do 
    val1 <- evalExp e1 
    val2 <- evalExp e2 

    case (val1, val2) of
        (Node leftValue leftDepth, Node rightValue rightDepth) -> do 
            if leftDepth > rightDepth then -- balancing Nodes on stack
                return $ 
                    Node 
                    (leftValue <> rightValue <> (fromString ((if isCommutative then "\t" else "\tswap\n\t") ++ op ++ "\n")))
                    (max leftDepth (rightDepth + 1))
            else 
                return $ 
                    Node 
                    (rightValue <> leftValue <> (fromString ((if isCommutative then "\t" else "\tswap\n\t") ++ op ++ "\n")))
                    (max (leftDepth + 1) rightDepth)

        _ -> error "Not possible to get there!"

--- STATEMENTS ---
evalStmt :: [I.Stmt] -> InstantJvmMonad Value

evalStmt (I.SAss (I.Ident newtag) exp : tstmts) = do 
    val <- evalExp exp 
    case val of 
        (Node value depth) -> do 
            modify (updateStackSize depth)

            st <- get
            case M.lookup newtag (variables st) of 
                Just ind -> do
                    tell $ (L.unpack (toLazyText value)) ++ (storeLineStr ind)
                    evalStmt tstmts

                Nothing -> do 
                    ind <- gets localSize
                    modify incLocalSize
                    modify (declareVar newtag ind)
                    tell $ (L.unpack (toLazyText value)) ++ (storeLineStr ind)
                    evalStmt tstmts

        _ -> error "Not possible to get there!" 


evalStmt ((I.SExp exp) : tstmts) = do 
    val <- evalExp exp 
    case val of 
        (Node value depth) -> do 
            modify (updateStackSize depth)
            tell $ (L.unpack (toLazyText value)) ++ cPRINT_COMMAND
            evalStmt tstmts
        _ -> error "Not possible to get there!" 


evalStmt [] = do
    finalLocalSize <- gets localSize
    finalStackSize <- gets stackSize
    -- Serialising information on stack and locals size
    tell $ "#" ++ show finalLocalSize ++ "#" ++ show finalStackSize
    return VBlank

runInstant :: I.Program -> String -> IO ()
runInstant (I.Prog stmts) filepath = do
    let st = St { variables =  M.empty, stackSize = 2, localSize = 1 } 
    let result = runMonad st (evalStmt stmts)
    let className = takeBaseName filepath
    let resultFile = replaceExtension filepath "j"
    let [resultStripped, localSize, stackSize] = map T.unpack (T.splitOn (T.pack "#") (T.pack result))
    writeFile resultFile (generateJvmFile resultStripped className localSize stackSize)
    
    _ <- system $ "java -jar lib/jasmin.jar " ++ resultFile ++ " -d " ++ (takeDirectory filepath)

    return ()