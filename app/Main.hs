import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode(..))
import System.Environment (getArgs)
import System.Process (readProcessWithExitCode)


main :: IO ()
main = do
    args <- getArgs
    case args of
        (branch:paths) -> run branch paths
        [] -> putStrLn "usage: bonsai add <branch> <files> ..."


run :: String -> [String] -> IO ()
run branch paths = do
    exists <- doesBranchExist branch
    if exists
        then putStrLn "error: branch exists"
        else do
            expandedPaths <- expandPaths paths
            putStrLn $ show expandedPaths
            shas <- mapM hashObject expandedPaths
            putStrLn $ show shas


doesBranchExist :: String -> IO Bool
doesBranchExist branch = do
    (code, _, _) <- readProcessWithExitCode "git" ["show-ref", "--verify", "refs/heads/" ++ branch] ""
    return $ code == ExitSuccess


expandPaths :: [String] -> IO [String]
expandPaths paths = concat <$> mapM expandPath paths
    where
        expandPath p = do
            isDir <- doesDirectoryExist p
            if isDir
                then expandPaths . map ((p ++ "/") ++) =<< listDirectory p
                else return [p]


hashObject :: String -> IO String
hashObject paths = do
    (_, stdout, _) <- readProcessWithExitCode "git" ["hash-object", "-w", paths] ""
    return $ head $ lines stdout
