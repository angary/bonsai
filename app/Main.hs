import Control.Monad (when)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode(..), exitFailure)
import System.Environment (getArgs)
import System.Process (readProcessWithExitCode)


data TreeEntry = TreeEntry 
    { mode :: String
    , entryType :: String
    , sha :: String
    , path :: String
    } deriving (Show)


main :: IO ()
main = do
    args <- getArgs
    when (null args) $ do
        putStrLn "usage: bonsai add <branch> <files> ..."
        exitFailure
    let (branch:paths) = args
    when (null paths) $ do
        putStrLn "error: no files specified"
        exitFailure
    run branch paths


run :: String -> [String] -> IO ()
run branch paths = do
    exists <- doesBranchExist branch
    -- TODO: Handle existing branch?
    when exists $ do
        putStrLn "error: branch already exists"
        exitFailure
    expandedPaths <- expandPaths paths
    shas <- mapM hashFile expandedPaths
    tree <- getTree
    putStrLn $ show tree


doesBranchExist :: String -> IO Bool
doesBranchExist branch = do
    (code, _, _) <- readProcessWithExitCode "git" ["show-ref", "--verify", "refs/heads/" ++ branch] ""
    return $ code == ExitSuccess


expandPaths :: [String] -> IO [String]
expandPaths paths = concat <$> mapM expandPath paths


expandPath :: String -> IO [String]
expandPath p = do
    isDir <- doesDirectoryExist p
    if isDir
        then expandPaths . map ((p ++ "/") ++) =<< listDirectory p
        else return [p]


hashFile :: String -> IO String
hashFile path = do
    (_, stdout, _) <- readProcessWithExitCode "git" ["hash-object", "-w", path] ""
    return $ head $ lines stdout


getTree :: IO [TreeEntry]
getTree = do -- TODO: Look into not hardcoding origin/main
    (_, stdout, _) <- readProcessWithExitCode "git" ["ls-tree", "origin/main"] ""
    return $ map parseTreeEntry $ lines stdout


parseTreeEntry :: String -> TreeEntry
parseTreeEntry line =
    let (meta, tabPath) = break (== '\t') line
        [m, t, s]       = words meta
    in TreeEntry m t s (drop 1 tabPath)
