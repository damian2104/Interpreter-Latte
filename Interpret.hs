module Interpret where

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

    type Env = DM.Map Ident ValueWithFlag

    type FEnv = DM.Map Ident TopDef

    data AllEnv = AllEnv Env FEnv


    evalExpr :: Expr -> AllEnv -> M Value

    evalExpr (ELitInt number) allenv = return $ (NumVal number)

    evalExpr (ELitTrue) allenv = return $ (BVal True)

    evalExpr (ELitFalse) allenv = return $ (BVal False)

    evalExpr (EString inscription) allenv = return $ (StrVal inscription)

    evalExpr (EVar id) allenv = do
        case allenv of 
            AllEnv env fenv -> do
                vwf <- seek allenv id
                case vwf of
                    ValueWithFlag value rd_only global -> return $ value

    evalExpr (Neg e) allenv = do
        a <- evalExpr e allenv
        case a of
            NumVal number -> return $ NumVal ((-1)*number)
            _ -> fail ("No integer number: " ++ show e)


    evalExpr (Not e) allenv = do
        a <- evalExpr e allenv
        case a of
            BVal True -> return $ BVal (False)
            BVal False -> return $ BVal (True)
            _ -> fail ("No bool value " ++ show e)


    evalExpr (EAdd expr1 addop expr2) allenv = do
        a <- evalExpr expr1 allenv
        b <- evalExpr expr2 allenv
        case a of
            NumVal n1 -> case b of
                NumVal n2 -> case addop of
                    Plus -> return $ NumVal (n1+n2)
                    Minus -> return $ NumVal (n1-n2)
                _ -> fail ("no numeric value in add: " ++ show a)
            _ -> fail ("no numeric value in add: " ++ show b)

    
    evalExpr (EMul expr1 mulop expr2) allenv = do
        a <- evalExpr expr1 allenv
        b <- evalExpr expr2 allenv
        case a of
            NumVal n1 -> case b of
                NumVal n2 -> case mulop of
                    Times -> return $ NumVal (n1*n2)
                    Div -> case n2 of
                        0 -> fail ("dividing by 0: " ++ show a ++ ", " ++ show b)
                        _ -> return $ NumVal (div n1 n2)
                    Mod -> case n2 of
                        0 -> fail ("dividing by 0: " ++ show a ++ ", " ++ show b)
                        _ -> return $ NumVal (mod n1 n2)
                _ -> fail "no numeric value"
            _ -> fail "no numeric value"

            
    evalExpr (ERel expr1 relop expr2) allenv = do
        a <- evalExpr expr1 allenv
        b <- evalExpr expr2 allenv

        case a of
            NumVal n1 -> case b of
                NumVal n2 -> case relop of
                    LTH -> return $ BVal (n1<n2)
                    LE -> return $ BVal (n1<=n2)
                    GTH -> return $ BVal (n1>n2)
                    GE -> return $ BVal (n1>=n2)
                    EQU -> return $ BVal (n1==n2)
                    NE -> return $ BVal (n1/=n2)
                _ -> fail "comparing different types"
            StrVal s1 -> case b of
                StrVal s2 -> case relop of
                    LTH -> return $ BVal (s1<s2)
                    LE -> return $ BVal (s1<=s2)
                    GTH -> return $ BVal (s1>s2)
                    GE -> return $ BVal (s1>=s2)
                    EQU -> return $ BVal (s1==s2)
                    NE -> return $ BVal (s1/=s2)
                _ -> fail "comparing different types"
            BVal b1 -> case b of
                BVal b2 -> case relop of
                    LTH -> return $ BVal (b1<b2)
                    LE -> return $ BVal (b1<=b2)
                    GTH -> return $ BVal (b1>b2)
                    GE -> return $ BVal (b1>=b2)
                    EQU -> return $ BVal (b1==b2)
                    NE -> return $ BVal (b1/=b2)
                _ -> fail "comparing different types"


    evalExpr (EApp id exprs) allenv = do
        case allenv of
            AllEnv env fenv -> do
                func <- findFunction id allenv
                newAllEnv <- runTopDef func exprs allenv
                retval <- seek newAllEnv (Ident "RET_VAL")
                case retval of
                    ValueWithFlag value flag global -> return value


    seek :: AllEnv -> Ident -> M ValueWithFlag
    seek (AllEnv env fenv) i = do 
        let vlf = DM.lookup i env  
        case vlf of
            Nothing -> fail ("No variable " ++ show i ++ " in env")
            Just x -> case x of
                ValueWithFlag val flag global -> return $ x
        

    seek2 :: AllEnv -> Ident -> M TopDef
    seek2 (AllEnv env fenv) i = case val of
            Nothing -> fail ("No variable " ++ show i ++ " in env")
            Just x -> return x
        where val = DM.lookup i fenv
    

    passInfo :: Env -> Env -> M Env
    passInfo env1 env2 = do
        let ifBreak = DM.lookup (Ident "BREAK") env1
        case ifBreak of
            Just a -> do
                let env3 = DM.insert (Ident "BREAK") a env2
                return $ env3
            Nothing -> do
                let ret = DM.lookup (Ident "RET_VAL") env1
                case ret of
                    Just c -> do
                        let env3 = DM.insert (Ident "RET_VAL") c env2
                        return $ env3
                    Nothing -> return $ env2
                    


    evalStmt :: Stmt -> AllEnv -> M AllEnv


    evalStmt Empty allenv = return $ allenv

    evalStmt (BStmt b) allenv = do
        case allenv of
            AllEnv env fenv -> do
                (AllEnv newEnv newFenv) <- evalBlock b allenv
                let newEnv2 = DM.intersection newEnv env
                newEnv3 <- passInfo newEnv newEnv2
                return $ (AllEnv newEnv3 fenv)

    evalStmt (Decl t it) allenv = do
        newAllEnv <- evalDecl t it False allenv
        return $ newAllEnv

    evalStmt (Prt expr) allenv = do
        val <- evalExpr expr allenv
        case val of
            NumVal num -> liftIO $ putStrLn  $ show num
            StrVal str -> liftIO $ putStrLn  $ str
            BVal bv -> liftIO $ putStrLn $ show bv
        return allenv


    evalStmt (Ass id expr) (AllEnv env fenv) = do
        val <- evalExpr expr (AllEnv env fenv)
        case val of 
            NumVal value -> do
                vwf <- seek (AllEnv env fenv) id
                case vwf of
                    ValueWithFlag value False global -> do 
                        let newEnv = DM.insert id (ValueWithFlag val False global) env
                        return (AllEnv newEnv fenv)
                    ValueWithFlag value True global -> fail ("trying to change RD_ONLY value: " ++ show value)
            StrVal value -> do
                vwf <- seek (AllEnv env fenv) id
                case vwf of
                    ValueWithFlag value False global -> do 
                        let newEnv = DM.insert id (ValueWithFlag val False global) env
                        return (AllEnv newEnv fenv)
                    ValueWithFlag value True global -> fail ("trying to change RD_ONLY value: " ++ show value)
            BVal value -> do
                vwf <- seek (AllEnv env fenv) id
                case vwf of
                    ValueWithFlag value False global -> do 
                        let newEnv = DM.insert id (ValueWithFlag val False global) env
                        return (AllEnv newEnv fenv)
                    ValueWithFlag value True global -> fail ("trying to change RD_ONLY value: " ++ show value)


    evalStmt (Cond expr stmt) allenv = do
        val <- evalExpr expr allenv
        case val of
            BVal False -> return $ allenv
            BVal True -> evalStmt stmt allenv
            _ -> fail "no bool value"


    evalStmt (CondElse expr stmt1 stmt2) allenv = do
        val <- evalExpr expr allenv
        case val of
            BVal True -> evalStmt stmt1 allenv
            BVal False -> evalStmt stmt2 allenv
            _ -> fail "no bool value"

    
    evalStmt (While expr stmt) allenv = do
        val <- evalExpr expr allenv
        case val of
            BVal False -> return $ allenv
            BVal True -> do
                (AllEnv env1 fenv1) <- evalStmt stmt allenv
                let ifBreak = DM.member (Ident "BREAK") env1
                case ifBreak of
                    False -> do
                        evalStmt (While expr stmt) (AllEnv env1 fenv1)
                    True -> do
                        let env2 = DM.delete (Ident "BREAK") env1
                        return (AllEnv env2 fenv1)
            _ -> fail "no bool value"

    
    evalStmt (For id expr1 expr2 stmt) (AllEnv env fenv) = do
        begin <- evalExpr expr1 (AllEnv env fenv)
        end <- evalExpr expr2 (AllEnv env fenv)
        let newEnv1 = DM.insert id ( ValueWithFlag begin True False) env
        (AllEnv newEnv2 newFenv2) <- evalFor id end stmt (AllEnv newEnv1 fenv)
        let newEnv3 = DM.delete id newEnv2
        return $ (AllEnv newEnv3 newFenv2)


    evalStmt (SExp expr) allenv = do
        e <- evalExpr expr allenv 
        return $ allenv

    
    evalStmt (Ret expr) (AllEnv env fenv) = do
        val <- evalExpr expr (AllEnv env fenv)
        let newEnv = DM.insert (Ident "RET_VAL") (ValueWithFlag val False False) DM.empty 
        return $ (AllEnv newEnv fenv)


    evalStmt (FunDecl topdef) (AllEnv env fenv) = do
        let alldef = FunDef topdef
        newFenv <- getFunctions [alldef] fenv
        return $ (AllEnv env newFenv)


    evalStmt (Brk) (AllEnv env fenv) = do
        let newEnv = DM.insert (Ident "BREAK") (ValueWithFlag (BVal True) False False) env
        return $ (AllEnv newEnv fenv)


    evalStmt (Cont) (AllEnv env fenv) = do
        let newEnv = DM.insert (Ident "CONTINUE") (ValueWithFlag (BVal True) False False) env
        return $ (AllEnv newEnv fenv)



    evalFor :: Ident -> Value -> Stmt -> AllEnv -> M AllEnv
    evalFor id end stmt (AllEnv env fenv) = do
        let i = DM.lookup id env
        case i of
            Just (ValueWithFlag (NumVal var) b g) -> case end of
                NumVal border -> 
                    if var <= border
                        then do
                            (AllEnv env2 fenv2) <- evalStmt stmt (AllEnv env fenv)
                            let env3 = DM.insert id (ValueWithFlag (NumVal (var+1)) b g) env2
                            evalFor id end stmt (AllEnv env3 fenv2) 
                                else return (AllEnv env fenv)
    

    checkIfReturnHappened :: AllEnv -> M Bool
    checkIfReturnHappened (AllEnv env fenv) = do 
        let x = DM.lookup (Ident "RET_VAL") env
        case x of
            Nothing -> do
                let y = DM.lookup (Ident "CONTINUE") env
                case y of
                    Nothing -> do
                        let z = DM.lookup (Ident "BREAK") env
                        case z of
                            Nothing -> return $ False
                            Just c ->  return $ True
                    Just b -> return $ True 
            Just a -> return $ True


    evalBlock :: Block -> AllEnv -> M AllEnv

    evalBlock (Block []) allEnv = return $ allEnv

    evalBlock (Block (h:hs)) (AllEnv env fenv) = do
        checkReturn <- checkIfReturnHappened (AllEnv env fenv)
        (AllEnv newEnv1 newFenv1) <- evalStmt h (AllEnv env fenv)
        checkReturn <- checkIfReturnHappened (AllEnv newEnv1 newFenv1)
        case checkReturn of
            True -> do
                let newEnv2 = DM.delete (Ident "CONTINUE") newEnv1
                return $ (AllEnv newEnv2 newFenv1)
            False -> do
                newAllEnv2 <- evalBlock (Block (hs)) (AllEnv newEnv1 newFenv1)
                return $ newAllEnv2


    evalDecl :: Type -> [Item] -> GLOBAL -> AllEnv -> M AllEnv
    evalDecl t [] global allenv = return $ allenv

    evalDecl t (h:hs) global (AllEnv env fenv) =
        evalDecl t hs global (AllEnv newEnv1 fenv) where
            newEnv1 = case t of
                Int -> DM.insert (getIdentfromItem h) (ValueWithFlag (NumVal 0) False global) env
                Str -> DM.insert (getIdentfromItem h) (ValueWithFlag (StrVal "") False global) env
                Bool -> DM.insert (getIdentfromItem h) (ValueWithFlag (BVal False) False global) env


    parseArgumentsToFunction :: TopDef -> [Expr] -> [Arg] -> Env -> AllEnv -> M AllEnv

    parseArgumentsToFunction topdef [] [] newEnv (AllEnv env fenv) = do
        return $ (AllEnv newEnv fenv)

    parseArgumentsToFunction topdef (e:es) (a:as) newEnv (AllEnv env fenv) = do
        val <- evalExpr e (AllEnv env fenv)
        case a of
            Arg t id -> do
                let newEnv2 = DM.insert id (ValueWithFlag val False False) newEnv
                parseArgumentsToFunction topdef es as newEnv2 (AllEnv env fenv)


        
    addIfGlobal :: [Ident] -> AllEnv -> Env -> M AllEnv
    addIfGlobal [] (AllEnv env fenv) newEnv = do
        return $ (AllEnv env fenv)

    addIfGlobal (key:keyTail) (AllEnv env fenv) newEnv = do
        vwf <- seek (AllEnv env fenv) key
        case vwf of
            ValueWithFlag value rd_only global -> case global of
                True -> do
                    let newEnv2 = DM.insert key vwf newEnv
                    addIfGlobal keyTail (AllEnv env fenv) newEnv2
                False -> addIfGlobal keyTail (AllEnv env fenv) newEnv


    addGlobalsToEnv :: AllEnv -> M AllEnv
    addGlobalsToEnv (AllEnv env fenv) = do
        let keys = DM.keys env
        addIfGlobal keys (AllEnv env fenv) DM.empty 


    runTopDef :: TopDef -> [Expr] -> AllEnv -> M AllEnv
    runTopDef topdef exprs (AllEnv env fenv) = do
        case topdef of
            FnDef t id args block -> do
                (AllEnv env1 fenv1) <- addGlobalsToEnv (AllEnv env fenv)
                allenv2 <- parseArgumentsToFunction topdef exprs args env1 (AllEnv env fenv)
                allenv3 <- evalBlock block allenv2
                return $ allenv3


    runProgram :: Program -> FEnv -> M FEnv
    runProgram program fenv = do
        case program of
            Program alldefs -> getFunctions alldefs fenv



    getGlobals :: [AllDef] -> AllEnv -> M AllEnv
    getGlobals alldefs allenv = do
        case alldefs of
            [] -> return allenv
            (h:hs) -> case h of
                VarDef topvar -> case topvar of
                    VarDecl t item -> do
                        newAllEnv <- evalDecl t item True allenv
                        getGlobals hs newAllEnv
                _ -> getGlobals hs allenv


    runProgram2 :: Program -> AllEnv -> M AllEnv
    runProgram2 program allenv = do
        case program of
            Program alldefs -> getGlobals alldefs allenv


    getFunctions :: [AllDef] -> FEnv -> M FEnv
    getFunctions alldefs fenv = do
        case alldefs of
            [] -> return fenv
            (h:hs) -> do
                case h of
                    FunDef topdef -> case topdef of
                        FnDef t id args block -> do
                            let newFenv = DM.insert id topdef fenv
                            getFunctions hs newFenv
                    _ -> getFunctions hs fenv


    getIdentfromItem :: Item -> Ident
    getIdentfromItem (NoInit id) = id


    interpret :: Program -> M Value
    interpret program = do
        fenv <- runProgram program DM.empty
        allEnv <- runProgram2 program (AllEnv DM.empty fenv)
        main <- findFunction (Ident "main") allEnv
        case main of
            FnDef t id args block -> do
                (AllEnv env2 fenv2) <- evalBlock block allEnv
                vwf <- seek (AllEnv env2 fenv2) (Ident "RET_VAL")
                case vwf of
                    ValueWithFlag value flag global -> return $ value


    run p = runExceptT (interpret p)

    findFunction :: Ident -> AllEnv -> M TopDef
    findFunction id allenv = do
        func <- seek2 allenv id
        return $ func


    getProgram s = do
        pProgram (myLexer s)