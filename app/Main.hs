import System.Exit (ExitCode(..))
import System.Environment (getArgs)
import System.Process (readProcessWithExitCode)


main :: IO ()
main = do
    args <- getArgs
    case args of
        (branch:paths) -> do
            exists <- doesBranchExist branch
            if exists
                then do
                    putStrLn "branch exists"
                else do
                    shas <- mapM hashObject paths
                    putStrLn $ show shas
        [] -> putStrLn "usage: bonsai add <branch> <files> ..."
    

doesBranchExist :: String -> IO Bool
doesBranchExist branch = do
    (code, _, _) <- readProcessWithExitCode "git" ["show-ref", "--verify", "refs/heads/" ++ branch] ""
    return $ code == ExitSuccess


-- TODO: Support hashing for directories (may need expansive recurison), atm only supported for files
hashObject :: String -> IO String
hashObject paths = do
    (_, stdout, _) <- readProcessWithExitCode "git" ["hash-object", "-w", paths] ""
    return $ head $ lines stdout
