import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (branch:files) -> putStrLn $ "Branch: " ++ branch ++ " Files: " ++ show files
        []             -> putStrLn "Usage: bonsai add <branch> <files> ..."

