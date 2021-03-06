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

-- Infix Expression
-- Evaluates two expressions connected by an operator.
-- Eg: e1 <= e2, e1 == e2, e1 + e2
-- [our code]
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2

-- Assign Expression
-- Evaluates an expression and assign its result to a given variable
-- Error: If no variable is defined, this returns an Error
-- [our code]
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    v <- stateLookup env var
    case v of
        -- Variable not defined :(, we initilize it and set its value
        (Error _) -> do
            varDecl env (VarDecl (Id var) Nothing)
            e<- evalExpr env expr
            setVar var e
        -- Variable defined, let's set its value
        _ -> do
            e <- evalExpr env expr
            setVar var e



-- StringLit
-- This Allows us to create an array of String
-- [our code]
evalExpr env (StringLit string) = return $ String string

-- BoolLit
-- This Allows us to create an array of String
-- [our code]
evalExpr env (BoolLit bool) = return $ Bool bool


-- ++i
-- Allows us to use an expression such as ++i
-- [our code]
evalExpr env (UnaryAssignExpr PrefixInc (LVar var)) = do
    v <- stateLookup env var
    case v of
        (Int val) -> do
            e <- return $ Int (val + 1)
            setVar var e
        _ -> return $ Error "Return value is not valid"

-- --i
-- Allows us to use an expression such as --i
-- [our code]
evalExpr env (UnaryAssignExpr PrefixDec (LVar var)) = do
    v <- stateLookup env var
    case v of
        (Int val) -> do
            e <- return $ Int (val - 1)
            setVar var e
        _ -> return $ Error "Return value is not valid"

-- i++
-- Allows us to use an expression such as i++
-- [our code]
evalExpr env (UnaryAssignExpr PostfixInc (LVar var)) = ST(\s ->
    let (ST f) = stateLookup env var
        (v, newS) = f s
    in case v of
        (Int val) -> let
                        (ST g)= setVar var (Int (val + 1))
                        (v2, newS2) = g newS
                        in (v, newS2)
        _ -> (Error "Return value is not valid", s) --devo realmente retornar s?
        )

-- i--
-- Allows us to use an expression such as i--
-- [our code]
evalExpr env (UnaryAssignExpr PostfixDec (LVar var)) = ST(\s ->
    let (ST f) = stateLookup env var
        (v, newS) = f s
    in case v of
        (Int val) -> let
                        (ST g)= setVar var (Int (val - 1))
                        (v2, newS2) = g newS
                        in (v, newS2)
        _ -> (Error "Return value is not valid", s) --devo realmente retornar s?
        )

-- Prefix Minus
-- This adds the feature that allows us to work with negative integers
-- [our code]
evalExpr env (PrefixExpr PrefixMinus expr) = do
    v <- evalExpr env expr
    case v of
        (Int val) -> return $ Int (-val)
        _ -> return $ Error "Return value is not valid"

--ArrayLit
-- Evaluates an Array
-- [our code]
evalExpr env (ArrayLit exprs) = do
    v <- mapM (evalExpr env) exprs
    return (Array v)

-- CallExpression
-- Evaluates a function call
-- [our code]

evalExpr env (CallExpr expr values)= ST (\s->
    let (ST f) = evalExpr env expr
        (v0, newS) = f s --PS: quando uma funÁ„o n„o È reconhecida, essa linha retorna erro
    in case v0 of
        (Func args cmds) ->
            let
                t= localList cmds
                tMap = fromList (createMapParams t) --mapeando var local
                (ST g) = initVarFunc env args values
                (v, newS2) = g newS
                (ST h) = evalStmt env (BlockStmt cmds)
                (v2, newS3) = h newS2
                newV2 = extractReturn v2
                newS4 = updateState newS newS3 (fromList (createMapParams args)) tMap
            in (newV2, newS4)
        (Native f) ->
                let
                (ST g) = mapM (evalExpr env) values
                (v, newS1) = g newS
                in ((f v), newS1)
        _ -> (Int 0, newS)
    )

--EvalVarRefLits
--evalExprAdpt env []= []
--evalExprAdpt env ((VarRef (Id id)):xs) = (stateLookup env id):(evalExprAdpt env xs)
--evalExprAdpt env ((ArrayLit array):xs) = (evalExpr env (ArrayLit array)):(evalExprAdpt env xs)


-- Given a global state, a local state and a param state,
-- 'updateState' gives us the new global state according to js scope definition
-- [our code]
updateState oldEnv newEnv paramsEnv localVar=
     let local1 = localVar
         local2 = intersection newEnv paramsEnv
         local = union local1 local2
         globalNotUpdated = difference newEnv local
         globalUpdated = union globalNotUpdated oldEnv
     in globalUpdated


-- Given a list of (Id v), this creates a list of (v, "")
-- [our code]
createMapParams [] = []
createMapParams ((Id p):ps) = (p, Nil): createMapParams ps


-- Extract Return
-- Extract the value inside a Return data type
-- [our cod]
extractReturn (Return x) = x
extractReturn v = v

--funções para rastrear variáveis globais
localList [] = []
localList (x:xs) = do
    case x of
        VarDeclStmt d -> (localList2 d )++localList xs
        IfSingleStmt expr (BlockStmt stmt) -> (localList (stmt))++(localList xs)
        IfStmt expr (BlockStmt stmt1) (BlockStmt stmt2) -> localList stmt1 ++ localList stmt2 ++ localList xs
        ForStmt (VarInit i) exp1 exp2 (BlockStmt stmt) ->(localList2 i)++(localList stmt)++(localList xs)
        ForStmt NoInit exp1 exp2 (BlockStmt stmt) ->localList (stmt)++(localList xs)
        _ -> localList xs

localList2 [] = []
localList2 ((VarDecl x m):xs) = x : localList2 xs


--Initializing several var for FunctionExpression
-- [our implementation]
initVarFunc env [] [] = return Nil
initVarFunc env [] _ = return $ Error "Size of parameters don't match"
initVarFunc env _ [] = return $ Error "Size of parameters don't match"
initVarFunc env ((Id ids):idsx) (v:vx)= do
    value<- evalExpr env v
    setVar ids value >> initVarFunc env idsx vx


evalStmt :: StateT -> Statement -> StateTransformer Value

--Already implemented by Paulo
evalStmt env EmptyStmt = return Nil
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
evalStmt env (ExprStmt expr) = evalExpr env expr


-- Return Statement
-- Evaluates an return statement
-- [our code]
evalStmt env (ReturnStmt expr) = do
    case expr of
        (Just x) -> do
            v <- evalExpr env x
            return $ Return v
        Nothing -> return $ Return Nil -- checar se nill está correto

-- BlockStatement
-- Evaluates a block statement
evalStmt env (BlockStmt (stmt:sx))= do
    v<- (evalStmt env stmt)
    case v of
        (Break) -> return Nill
        (Return x) -> return (Return x)
        (_) -> evalStmt env (BlockStmt sx)
evalStmt env (BlockStmt []) = return Nil

-- IfSingleStatement
-- Evaluates a single if statement
-- Ex: if (3>2) then cmd;
-- Error: Returns an error when the condition is invalid
evalStmt env (IfSingleStmt expr cmd) = do
    v<-evalExpr env expr
    case v of
        --Case v is a Boolean
        (Bool b) -> if b then evalStmt env cmd else return Nil
        --Case v is an Error
        (Error _) -> return $ Error "Error"
        (_) -> return $ Error "this is not a valid stmt"

-- IfStatement
-- Evaluates an if-then-else statement
-- Ex: if (3>2) then cmd1 else cmd2;
-- Error: Returns an error when the condition is invalid
evalStmt env (IfStmt cond trueblock falseblock) = do
    v <- evalExpr env cond
    case v of
        (Bool v1) -> if v1 then evalStmt env trueblock else evalStmt env falseblock
        --Case Error
        (Error _) -> return $ Error "Error"
        (_) -> return $ Error "this is not a valid stmt"

--FunctionStatement
-- Evaluates a Funcrion
evalStmt env (FunctionStmt (Id name) arg cmd)= do
    newF <- return (Func arg cmd)
    setVar name newF

-- ForStatement [escopo local por algum motivo não está funcionando com o break]
-- Allows us to use the for structure
-- Error:
-- [our code]
--New For with global variables!!!!!
evalStmt env (ForStmt i expr iExpr cmd)= do
    v1<-initFor env i
    v2<-evalExprMaybe env expr
    case v2 of
        (Bool True) ->do
            v3<-evalStmt env cmd
            case v3 of
                (Break) -> return Break
                (Return v) -> return $ Return v
                (_) -> evalExprMaybe env iExpr >> evalStmt env (ForStmt NoInit expr iExpr cmd)
        (Bool False)-> return Nil
        (Error _) -> return $ Error "Error"
        (_) -> return (Error "this is not a valid stmt")


-- BreakStatement
-- When a break is detected, it creates a new indirection to
-- represent the break statement
-- [our code]
evalStmt env (BreakStmt i)= do
    case i of
        Nothing -> return Break --Case we want to break the for
        (Just i) -> return $ Error "Error while processing a break stmt"

--Initializing ForInit --!ERRor test it!
initFor:: StateT -> ForInit -> StateTransformer Value
initFor env  i = do
 case i of
        (NoInit) -> return Nil --StateTransformer Value
        (VarInit var) -> evalInit env var  --StateTransformer Value
        (ExprInit expr) -> evalExpr env expr --StateTransformer Value


evalInit:: StateT -> [VarDecl] -> StateTransformer Value
evalInit env []= return Nil
evalInit env (x:xs) =
    varDecl env x >> evalInit env xs

evalExprMaybe:: StateT -> Maybe Expression -> StateTransformer Value
evalExprMaybe env expr=
    case expr of
        Nothing -> return (Bool True) --StateTrasformer Value
        (Just expr) -> evalExpr env expr --StateTrasformer Value

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
infixOp env OpEq   (String  v1) (String  v2) = return $ Bool $ v1 == v2
infixOp env OpEq  (Array  v1) (Array  v2) = return $ Bool  $ eqArray v1 v2
infixOp env OpEq  (Bool  v1) (Bool  v2) = return $ Bool  $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpNEq   (Int  v1) (Int  v2) = return $ Bool $ v1 /= v2
infixOp env OpNEq   (String  v1) (String  v2) = return $ Bool $ v1 /= v2
infixOp env OpNEq  (Array  v1) (Array  v2) = return $ Bool  $ not(eqArray v1 v2)
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

infixOp env op _ _ = return $ Error "It was not possible to proceed with the evaluation"

--
-- Environment and auxiliary functions
--

environment :: Map String Value
environment = insert "head" (Native vHead)
              $insert "tail" (Native vTail)
              $insert "concat" (Native vConcat)
              empty

eqArray [] [] = True
eqArray _ [] = False
eqArray [] _ = False
eqArray ((Int v1):vs1) ((Int v2):vs2) = (v1 == v2) && (eqArray vs1 vs2)
eqArray ((String v1):vs1) ((String v2):vs2) = (v1 == v2) && (eqArray vs1 vs2)

vTail ((Array vList):xs) = Array (tail vList)
vTail _ = Error "argumentos invalidos. A entrada nao eh uma lista"


vHead ((Array vList):xs)= head vList
vHead a = Error "argumentos invalidos. A entrada nao eh uma lista"


vConcat [(Array values1), (Array values2)] = Array (values1++values2)
vConcat _ = Error "argumentos invalidos. A entrada nao eh uma lista"

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
showResult (val, defs) = do
    case val of
        Error v -> show(Error v)++ "\n"
        _-> show val ++ "\n" ++ show (toList defs) ++ "\n"

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f empty

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
