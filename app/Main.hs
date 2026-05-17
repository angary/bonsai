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
    -- TODO: Auto add modified files to new branch if null?
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
    shas          <- mapM hashFile expandedPaths
    tree          <- getTree
    let finalTree  = buildTree expandedPaths shas tree
    let treeInput  = unlines $ map formatTreeEntry finalTree
    treeSha       <- mkTree treeInput
    commitSha     <- createCommit treeSha
    createBranch branch commitSha
    push branch
    restoreFiles expandedPaths
    putStrLn $ "pushed branch: " ++ branch


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


-- TODO (IMPORTANT): Support subdirectories
-- TODO: Investigate non-hardcoded origin/main
getTree :: IO [TreeEntry]
getTree = do
    (_, stdout, _) <- readProcessWithExitCode "git" ["ls-tree", "origin/main"] ""
    return $ map parseTreeEntry $ lines stdout


parseTreeEntry :: String -> TreeEntry
parseTreeEntry line =
    let (meta, tabPath) = break (== '\t') line
        [m, t, s]       = words meta
    in TreeEntry m t s (drop 1 tabPath)


buildTree :: [String] -> [String] -> [TreeEntry] -> [TreeEntry]
buildTree paths shas tree = 
    let pathShas    = zip paths shas
        updatedTree = map (updateTreeEntry pathShas) tree
        newEntries  = [TreeEntry "100644" "blob" s p | (p, s) <- pathShas, p `notElem` map path tree]
    in updatedTree ++ newEntries


updateTreeEntry :: [(String, String)] -> TreeEntry -> TreeEntry
updateTreeEntry pathShas entry =
    case lookup (path entry) pathShas of
        Just newSha -> entry { sha = newSha }
        Nothing     -> entry


formatTreeEntry :: TreeEntry -> String
formatTreeEntry e  = mode e ++ " " ++ entryType e ++ " " ++ sha e ++ "\t" ++ path e


mkTree :: String -> IO String
mkTree treeInput = do
    (_, stdout, _) <- readProcessWithExitCode "git" ["mktree"] treeInput
    return $ head $ lines stdout


createCommit :: String -> IO String
createCommit treeSha = do
    -- TODO: Current default commit message is "." (my convention), can open to user input
    (_, stdout, _) <- readProcessWithExitCode "git" ["commit-tree", treeSha, "-p", "origin/main", "-m", "."] ""
    return $ head $ lines stdout


createBranch :: String -> String -> IO ()
createBranch branch commitSha = do
    (code, stdout, stderr) <- readProcessWithExitCode "git" ["update-ref", "refs/heads/" ++ branch, commitSha] ""
    putStr stdout
    putStr stderr
    when (code /= ExitSuccess) exitFailure


push :: String -> IO ()
push branch = do
    (code, stdout, stderr) <- readProcessWithExitCode "git" ["push", "origin", branch] ""
    putStr stdout
    putStr stderr
    when (code /= ExitSuccess) exitFailure


restoreFiles :: [String] -> IO ()
restoreFiles paths = do
    (code, _, stderr) <- readProcessWithExitCode "git" (["checkout", "HEAD", "--"] ++ paths) ""
    putStr stderr
    when (code /= ExitSuccess) exitFailure
