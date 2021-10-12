module TypeControl where
    import System.IO
    import Lang.Abs
    import Lang.Lex
    import Lang.Par
    import Lang.ErrM
    import Data.Map as DM
    import Data.Set as DS
    import Control.Monad.IO.Class
    import Control.Monad
    import Control.Monad.Except

    type M a = (ExceptT String IO a)

    type TEnv = DM.Map Ident Type

    type TFEnv = DM.Map Ident FType

    data FType = FType Type [Type] deriving Show

    data TAllEnv = TAllEnv TEnv TFEnv


    typeCheckExpr :: Expr -> TAllEnv -> M Type
    typeCheckExpr (EVar id) tallenv = do
        case tallenv of 
            TAllEnv env fenv -> do
                type_ <- seek tallenv id
                return $ type_

            
    typeCheckExpr (ELitInt int) tallenv = do
        return $ Int

    typeCheckExpr (ELitTrue) tallenv = do
        return $ Bool

    typeCheckExpr (ELitFalse) tallenv = do
        return $ Bool

    typeCheckExpr (EString str) tallenv = do
        return $ Str

    typeCheckExpr (Neg e) tallenv = do
        eType <- typeCheckExpr e tallenv
        case eType of
            Int -> return $ Int
            _ -> fail "no integer in neg expression"


    typeCheckExpr (Not e) tallenv = do
        eType <- typeCheckExpr e tallenv
        case eType of
            Bool -> return $ Bool
            _ -> fail "no bool in not expression"


    typeCheckExpr (EAdd expr1 addop expr2) tallenv = do
        a <- typeCheckExpr expr1 tallenv
        b <- typeCheckExpr expr2 tallenv
        case a of
            Int -> case b of
                Int -> return Int
                _ -> fail "not numeric value in add operation"
            _ -> fail "not numeric value in add operation"


    typeCheckExpr (EMul expr1 addop expr2) tallenv = do
        a <- typeCheckExpr expr1 tallenv
        b <- typeCheckExpr expr2 tallenv
        case a of
            Int -> case b of
                Int -> return Int
                _ -> fail "not numeric value in mul operation"
            _ -> fail "not numeric value in mul operation"
    

    typeCheckExpr (ERel expr1 relop expr2) tallenv = do
        a <- typeCheckExpr expr1 tallenv
        b <- typeCheckExpr expr2 tallenv
        if (a == b) then
            return $ Bool
        else
            fail "comparing different types"


    typeCheckExpr (EApp id exprs) tallenv = do
        (FType retvalType args) <- findFunction id tallenv
        checkFunctionArgumentsType exprs args tallenv
        return $ retvalType


    typeCheckStmt :: Stmt -> TAllEnv -> M TAllEnv

    typeCheckStmt Empty tallenv = return $ tallenv

    typeCheckStmt (BStmt b) (TAllEnv tenv tfenv) = do
        (TAllEnv tenv2 tfenv2) <- typeCheckBlock b (TAllEnv tenv tfenv)
        let tenv3 = DM.intersection tenv2 tenv
        return $ (TAllEnv tenv3 tfenv)


    typeCheckStmt (Decl t it) tallenv = do
        tallenv2 <- typeCheckDecl t it tallenv
        return $ tallenv2


    typeCheckStmt (Prt expr) tallenv = do
        return $ tallenv


    typeCheckStmt (Ass id expr) tallenv = do
        typeExpr <- typeCheckExpr expr tallenv
        typeId <- seek tallenv id

        if (typeExpr == typeId) then
            return $ tallenv
        else
            fail "assignment: not compatible types"


    typeCheckStmt (Cond expr stmt) tallenv = do
        typeExpr <- typeCheckExpr expr tallenv
        if (typeExpr == Bool) then
            typeCheckStmt stmt tallenv
        else
            fail "No logical expression in condition if"
        return $ tallenv


    typeCheckStmt (CondElse expr stmt1 stmt2) tallenv = do
        typeExpr <- typeCheckExpr expr tallenv
        if (typeExpr == Bool) then do
            typeCheckStmt stmt1 tallenv
            typeCheckStmt stmt2 tallenv
        else
            fail "No logical expression in condition if-else"
        return $ tallenv


    typeCheckStmt (While expr stmt) tallenv = do
        typeExpr <- typeCheckExpr expr tallenv
        if (typeExpr == Bool) then do
            typeCheckStmt stmt tallenv
        else
            fail "No logical expression in loop while"

    
    typeCheckStmt (For id expr1 expr2 stmt) (TAllEnv tenv tfenv) = do
        typeExpr1 <- typeCheckExpr expr1 (TAllEnv tenv tfenv)
        typeExpr2 <- typeCheckExpr expr2 (TAllEnv tenv tfenv)

        if (typeExpr1 == typeExpr2) then
            if (typeExpr1 == Int) then do
                let tenv2 = DM.insert id Int tenv 
                tallenv2 <- typeCheckStmt stmt (TAllEnv tenv2 tfenv)
                return $ tallenv2
            else fail "no int expression in loop for"
        else fail "no int expression in loop for"


    typeCheckStmt (SExp expr) tallenv = do
        typeCheckExpr expr tallenv
        return $ tallenv


    typeCheckStmt (Ret expr) tallenv = do
        typeCheckExpr expr tallenv
        return $ tallenv


    typeCheckStmt (FunDecl topdef) (TAllEnv tenv tfenv) = do
        let alldef = FunDef topdef
        tfenv2 <- getFunctions [alldef] tfenv
        return $ (TAllEnv tenv tfenv2)


    typeCheckStmt (Brk) tallenv = do
        return $ tallenv


    typeCheckStmt (Cont) tallenv = do
        return $ tallenv


    
    

    typeCheck :: Program -> M Int
    typeCheck program = do
        tfenv <- takeAllFunctions program DM.empty
        tallenv <- takeGlobals program (TAllEnv DM.empty tfenv)
        x <- checkAllFunctions program tallenv
        return $ 0
        

    

    checkAllFunctions :: Program -> TAllEnv -> M TAllEnv 
    checkAllFunctions (Program []) tallenv = do
        return $ tallenv

    checkAllFunctions (Program (h:hs)) tallenv = do
        case h of
            FunDef topdef -> do
                checkFunction topdef tallenv
                checkAllFunctions (Program hs) tallenv
            _ -> checkAllFunctions (Program hs) tallenv


    addArgumentsToFunction :: [Arg] -> TAllEnv -> M TAllEnv
    addArgumentsToFunction [] tallenv = return $ tallenv

    addArgumentsToFunction (h:hs) (TAllEnv tenv tfenv) = do
        case h of
            Arg t id -> do
                let tenv2 = DM.insert id t tenv
                addArgumentsToFunction hs (TAllEnv tenv2 tfenv)


    checkFunction :: TopDef -> TAllEnv -> M TAllEnv
    checkFunction (FnDef t id args block) tallenv = do
        tallenv2 <- addArgumentsToFunction args tallenv
        typeCheckBlock block tallenv2
        



    -- ADDITIONAL FUNCTIONS

    takeGlobals :: Program -> TAllEnv -> M TAllEnv
    takeGlobals program tallenv = do
        case program of
            Program alldefs -> getGlobals alldefs tallenv


    getGlobals :: [AllDef] -> TAllEnv -> M TAllEnv
    getGlobals alldefs tallenv = do
        case alldefs of
            [] -> do
                return $ tallenv
            (h:hs) -> case h of
                VarDef topvar -> case topvar of
                    VarDecl t item -> do
                        tallenv2 <- typeCheckDecl t item tallenv
                        getGlobals hs tallenv2
                FunDef topdef -> getGlobals hs tallenv


    takeAllFunctions :: Program -> TFEnv -> M TFEnv
    takeAllFunctions (Program alldefs) tfenv = do
        getFunctions alldefs tfenv


    getFunctions :: [AllDef] -> TFEnv -> M TFEnv
    getFunctions alldefs tfenv = do
        case alldefs of
            [] -> return tfenv
            (h:hs) -> case h of
                FunDef topdef -> case topdef of
                    FnDef t id args block -> do
                        argTypes <- getArgs args []
                        let tfenv2 = DM.insert id (FType t argTypes) tfenv
                        getFunctions hs tfenv2
                _ -> getFunctions hs tfenv


    getArgs :: [Arg] -> [Type] -> M [Type]
    getArgs [] list = return $ list

    getArgs (h:hs) list = case h of
        Arg t id -> do
            let list2 = list ++ [t]
            list3 <- getArgs hs list2
            return $ list3


    typeCheckBlock :: Block -> TAllEnv -> M TAllEnv

    typeCheckBlock (Block []) tallenv = return $ tallenv

    typeCheckBlock (Block (h:hs)) (TAllEnv tenv tfenv) = do
        tallenv2 <- typeCheckStmt h (TAllEnv tenv tfenv)
        tallenv3 <- typeCheckBlock (Block hs) tallenv2
        return $ tallenv3


    typeCheckDecl :: Type -> [Item] -> TAllEnv -> M TAllEnv
    typeCheckDecl typee [] tallenv = return $ tallenv

    typeCheckDecl typee (h:hs) (TAllEnv tenv tfenv) = do
        let id = getIdentFromItem h
        let tenv2 = DM.insert id typee tenv
        tallenv2 <- typeCheckDecl typee hs (TAllEnv tenv2 tfenv)
        return $ tallenv2



        
    getIdentFromItem :: Item -> Ident
    getIdentFromItem (NoInit id) = id



    seek :: TAllEnv -> Ident -> M Type
    seek (TAllEnv env fenv) i = do 
        let typee = DM.lookup i env  
        case typee of
            Nothing -> fail ("No variable " ++ show i ++ " in env")
            Just x -> return $ x


    getTypeFromValue :: Value -> M Type
    getTypeFromValue val = do
        case val of
            NumVal x -> return $ Int
            StrVal x -> return $ Str
            BVal x -> return $ Bool


    findFunction :: Ident -> TAllEnv -> M FType
    findFunction id tallenv = do
        func <- seek2 tallenv id
        return $ func


    seek2 :: TAllEnv -> Ident -> M FType
    seek2 (TAllEnv tenv tfenv) i = case val of
            Nothing -> fail ("No function " ++ show i ++ " in env")
            Just x -> return x
        where val = DM.lookup i tfenv
        

    
    checkFunctionArgumentsType :: [Expr] -> [Type] -> TAllEnv -> M Bool

    checkFunctionArgumentsType [] [] tallenv = return $ True

    checkFunctionArgumentsType args [] tallenv = fail "Bad number of arguments parsed to function"

    checkFunctionArgumentsType [] fargs tallenv = fail "Bad number of arguments parsed to function"

    checkFunctionArgumentsType (argH:argT) (fArgH:fArgT) tallenv = do
        arg <- typeCheckExpr argH tallenv
        if (arg == fArgH) then do
            checkFunctionArgumentsType argT fArgT tallenv
            return $ True
        else
            fail "Bad type of argument parsed to function"


    check p = runExceptT (typeCheck p)

