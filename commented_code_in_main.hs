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
