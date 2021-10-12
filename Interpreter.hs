module Main where

    import Lang.Lex
    import Lang.Par
    import Lang.Abs
    import Interpret
    import System.Environment
    import Lang.ErrM
    import TypeControl

    main = do
        args <- System.Environment.getArgs
        case args of
            (h:hs) -> do 
                f <- readFile h
                let p = getProgram (f)
                case p of
                    Ok e -> do
                        a <- check e
                        case a of
                            Right x -> do
                                b <- run e
                                case b of
                                    Right y -> return()
                                    Left z -> print z
                            Left s -> print s
                    Bad s -> do
                        putStrLn("You shall not parse!")
                        putStrLn(show s)