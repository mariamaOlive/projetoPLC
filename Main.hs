import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad hiding (empty)
import Control.Applicative hiding (empty)
import Data.Map as Map
import Debug.Trace
import Value

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
--Implemented by Paulo
evalExpr env (VarRef (Id id)) = stateLookup env id

evalExpr env (IntLit int) = return $ Int int

evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2

evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    v <- stateLookup env var
    case v of
        -- Variable not defined :(
        (Error _) -> return $ Error $ (show var) ++ " not defined"
        -- Variable defined, let's set its value
        _ -> do
            e <- evalExpr env expr
            setVar var e


--PrefixExpression
evalExpr env (PrefixExpr PrefixMinus expr) = do
    v <- evalExpr env expr
    case v of 
        (Int val) -> return $ Int (-val)
        _ -> return $ Error "Return value is not valid"

--ArrayLit
evalExpr env (ArrayLit exprs) = 
    let newExprs = Prelude.map fst $ Prelude.map (\s -> getResult (evalExpr env s)) exprs
    in return $ Array newExprs

--FunctionExpression
evalExpr env (CallExpr expr values)= ST (\s->
    let (ST f) = evalExpr env expr
        ((Func args cmds), newS) = f s
        (ST g) =initVarFunc env args values
        (v, newS2) = g newS
        (ST h) = evalStmt env (BlockStmt cmds)
        (v2, newS3) = h newS2
        newV2 = extractReturn v2 
        newS4 = updateState newS newS3 (fromList (createMapParams args))
    in (newV2, newS4))

updateState oldEnv newEnv paramsEnv =
    let local1 = difference newEnv oldEnv
        local2 = intersection newEnv paramsEnv
        local = union local1 local2
        globalNotUpdated = difference newEnv local
        globalUpdated = union globalNotUpdated oldEnv
    in globalUpdated

createMapParams [] = []
createMapParams ((Id p):ps) = (p, ""): createMapParams ps


extractReturn (Return x) = x
extractReturn v = v

{-
evalExpr env (CallExpr expr values)= ST (\s->
    let (ST f) = evalExpr env expr
        ((Func args cmds), newS) = f s
        (ST g) =initVarFunc env args values
        (v, newS2) = let
                    (vx, newSx) = g newS
                    in case vx of
                        (Error e) = return $ Error e
                        _ = return (vx, newSx)
        (ST h) = evalStmt env (BlockStmt cmds)
        (v2, newS3) = h newS2
        newS4 = updateState newS newS3
    in (v2, newS4))
-}

{-updateState oldEnv newEnv =
    let local = difference newEnv oldEnv
        globalNotUpdated = difference newEnv local
        globalUpdated = union globalNotUpdated oldEnv
    in globalUpdated
 -}

    {-(Func args cmds)<- evalExpr env expr --retorna um ST e temos que pegar os args
    initVarFunc env args values
    evalStmt env (BlockStmt cmds)-}

--Initializing several var for FunctionExpression
--TODO: ao tratar de variáveis locais e globais estamos mandando listas com tamanhos diferentes!!!
--TODO: Antes: initVarFunc env [] [] = return Nil
--TODO: Quando descobrircomos como resolver o problema, adicionar a linha de cima novamente, para testar corretude
--initVarFunc env _ [] = return Nil
initVarFunc env [] [] = return Nil
initVarFunc env [] _ = return $ Error "Size of parameters don't match"
initVarFunc env _ [] = return $ Error "Size of parameters don't match"
initVarFunc env ((Id ids):idsx) (v:vx)= do
    value<- evalExpr env v
    setVar ids value >> initVarFunc env idsx vx


--PrefixExpression
{-evalExpr env (PrefixExpr op expr) = do
    op1 <-
-}


evalStmt :: StateT -> Statement -> StateTransformer Value

--Already implemented by Paulo
evalStmt env EmptyStmt = return Nil

evalStmt env (VarDeclStmt []) = return Nil

evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)

evalStmt env (ExprStmt expr) = evalExpr env expr


evalStmt env (ReturnStmt expr) = do
    case expr of
        (Just x) -> do
            v <- evalExpr env x
            return $ Return v
        Nothing -> return $ Return Nil -- checar se nill está correto


--Our implementation
--BlockStatement
evalStmt env (BlockStmt (stmt:sx))= do
    v<- trace ("avaliei") $ (evalStmt env stmt)
    case v of
        (Break) -> trace ("break") $return Break
        (Return x) -> trace ("return: " ++ show x) $ return (Return x)
        (_) -> trace ("(_)" ++ show v) $ evalStmt env (BlockStmt sx)
evalStmt env (BlockStmt []) = trace ("entrei aqui") $ return Nil

--IfSingleStatement
evalStmt env (IfSingleStmt expr cmd) = do
    v<-evalExpr env expr
    case v of
        --Case v is a Boolean
        (Bool b) -> if b then evalStmt env cmd else return Nil
        --Case v is an Error
        (Error _) -> return $ Error "Error"
        (_) -> return $ Error "this is not a valid stmt"

-- IfStatement - ADICIONANDO O IF THEN ELSE
evalStmt env (IfStmt cond trueblock falseblock) = do
    v <- evalExpr env cond
    case v of
        (Bool v1) -> if v1 then evalStmt env trueblock else evalStmt env falseblock
        --Case Error
        (Error _) -> return $ Error "Error"
        (_) -> return $ Error "this is not a valid stmt"

--FunctionStatement
evalStmt env (FunctionStmt (Id name) arg cmd)= do
    newF <- return (Func arg cmd)
    setVar name newF

-- ForStatement [escopo local por algum motivo não está funcionando com o break]
evalStmt env (ForStmt i expr iExpr cmd)= ST (\s->
    let (ST f1) = initFor env i
        (v1, newS1) = f1 s
        (ST f2) = evalExprMaybe env expr
        (v2, newS2) = f2 newS1
    in case v2 of
        (Bool True) ->
            let (ST f3) = evalStmt env cmd
                (v3, newS3) = f3 newS2
            in case v3 of
                   (Break) -> 
                                    let newSBreak = updateState s newS3 (difference newS1 s) 
                                    in (Nil, newSBreak)
                   (_) ->
                            let (ST f4) = evalExprMaybe env iExpr
                                (v4, newS4) = f4 newS3
                                (ST f5) = evalStmt env (ForStmt NoInit expr iExpr cmd)
                                (v5, newS5) = f5 newS4
                                newSFinal = updateState s newS5 (difference newS1 s)
                            in (v5, newSFinal)
        (Bool False)-> (Nil, s) --checar se é s
        (Error _) -> ((Error "Error"), s) --checar se é s
        (_) -> ((Error "this is not a valid stmt"), s) --checar se é s
    )
-- BreakStatement
evalStmt env (BreakStmt i)= do
    case i of
        Nothing -> return Break --Case we want to break the for
        (Just i) -> return $ Error "Error"

--Initializing ForInit --!ERRor test it!
initFor:: StateT -> ForInit -> StateTransformer Value
initFor env  i = do
 case i of
        (NoInit) -> return Nil --StateTransformer Value

        (VarInit var) -> evalInit env var  --StateTransformer Value

        (ExprInit expr) -> evalExpr env expr --StateTransformer Value

        (_) -> return $ Error "problems Initializing var" --StateTransformer Value


evalInit:: StateT -> [VarDecl] -> StateTransformer Value
evalInit env []= return Nil
evalInit env (x:xs) =
    varDecl env x >> evalInit env xs

evalExprMaybe:: StateT -> Maybe Expression -> StateTransformer Value
evalExprMaybe env expr=
    case expr of
        Nothing -> return (Bool True) --StateTrasformer Value

        (Just expr) -> evalExpr env expr --StateTrasformer Value

{-forSeq env cmd iExpr = do
   v1<- evalStmt env cmd
   v2<- evalExprMaybe env iExpr
   evalStmt env (ForStmt i expr iExpr cmd)
-}
{-evalExprFor env v2 cmd = do
    case v2 of
        (Bool b) -> if b then evalStmt env cmd else return Nil
        (Error _) -> return $ Error "Error"
        (_) -> return $ Error "this is not a valid stmt"
-}

{-evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt (a:as)) = do
    evalStmt env a
    evalStmt env (BlockStmt as)
-}



-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env [stmt] = evalStmt env stmt
evaluate env (s:ss) = evalStmt env s >> evaluate env ss

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

infixOp env op (Var x) v2 = do
    var <- stateLookup env x
    case var of
        error@(Error _) -> return error
        val -> infixOp env op val v2

infixOp env op v1 (Var x) = do
    var <- stateLookup env x
    case var of
        error@(Error _) -> return error
        val -> infixOp env op v1 val

infixOp env op (Return v) v1 = do
    trace ("eh uma funcao") $ return $ Error "a"

--
-- Environment and auxiliary functions
--

environment :: Map String Value
environment = empty

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    (maybe
        (Error $ "Variable " ++ show var ++ " not defined")
        id
        (Map.lookup var (union s env)),
    s)

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> setVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            setVar id val

--Function declaration
{-functionDecl:: StateT -> Stamentment -> StateTransformer Value
functionDecl env stmt = evalStmt env stmt
-}

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, insert var val s)

--
-- Types and boilerplate
--

type StateT = Map String Value
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS



instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, defs) = show val ++ "\n" ++ show (toList defs) ++ "\n"

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f empty

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
