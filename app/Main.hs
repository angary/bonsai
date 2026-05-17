import Control.Monad (when, foldM)
import Data.List (partition, sortBy)
import Data.Ord (comparing, Down(..))
import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory (doesDirectoryExist, listDirectory, removeFile)
import System.Exit (ExitCode(..), exitFailure)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, takeFileName)
import System.Process (readProcessWithExitCode)


newtype Sha    = Sha    { unSha    :: String } deriving (Show, Eq)
newtype Branch = Branch { unBranch :: String } deriving (Show, Eq)
newtype Path   = Path   { unPath   :: String } deriving (Show, Eq, Ord)

data TreeEntry = TreeEntry
    { mode      :: String
    , entryType :: String
    , sha       :: Sha
    , path      :: Path
    } deriving (Show)


runGit :: [String] -> String -> IO String
runGit args stdin = do
    (code, stdout, stderr) <- readProcessWithExitCode "git" args stdin
    putStr stderr
    when (code /= ExitSuccess) exitFailure
    return stdout


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
    run (Branch branch) (map Path paths)


run :: Branch -> [Path] -> IO ()
run branch paths = do
    exists <- doesBranchExist branch
    -- TODO: Handle existing branch?
    when exists $ do
        putStrLn "error: branch already exists"
        exitFailure
    expandedPaths <- expandPaths paths
    shas          <- mapM hashFile expandedPaths
    tree          <- getTree
    let pathShas   = zip expandedPaths shas
        dirMap     = groupByDirectory tree
    newShas       <- rebuildTrees dirMap pathShas
    let treeSha    = Map.findWithDefault (Sha "") (Path ".") newShas

    commitSha     <- createCommit treeSha
    createBranch branch commitSha
    push branch
    restoreFiles expandedPaths tree
    putStrLn $ "pushed branch: " ++ unBranch branch


doesBranchExist :: Branch -> IO Bool
doesBranchExist (Branch branch) = do
    (code, _, _) <- readProcessWithExitCode "git" ["show-ref", "--verify", "refs/heads/" ++ branch] ""
    return $ code == ExitSuccess


expandPaths :: [Path] -> IO [Path]
expandPaths paths = concat <$> mapM expandPath paths


expandPath :: Path -> IO [Path]
expandPath (Path p) = do
    isDir <- doesDirectoryExist p
    if isDir
        then expandPaths . map (Path . ((p ++ "/") ++)) =<< listDirectory p
        else return [Path p]


hashFile :: Path -> IO Sha
hashFile (Path filePath) = Sha . head . lines <$> runGit ["hash-object", "-w", filePath] ""


-- TODO: Investigate non-hardcoded origin/master
getTree :: IO [TreeEntry]
getTree = map parseTreeEntry . filter (not . null) . lines <$> runGit ["ls-tree", "-r", "origin/master"] ""


groupByDirectory :: [TreeEntry] -> Map Path [TreeEntry]
groupByDirectory = foldr insertEntry Map.empty
  where
    insertEntry entry acc =
        let dir = Path . takeDirectory . unPath $ path entry
        in Map.insertWith (++) dir [entry] acc



updateSubtree :: Map Path Sha -> TreeEntry -> TreeEntry
updateSubtree newShas entry =
    case Map.lookup (path entry) newShas of
        Just newSha -> entry { sha = newSha }
        Nothing     -> entry


rebuildTrees :: Map Path [TreeEntry] -> [(Path, Sha)] -> IO (Map Path Sha)
rebuildTrees dirMap pathShas = foldM rebuildDir Map.empty dirs
  where
    dirs = sortBy (comparing (Down . length . filter (== '/') . unPath)) (Map.keys mergedMap)

    newEntries = [ (Path . takeDirectory . unPath $ p, TreeEntry "100644" "blob" s p)
                 | (p, s) <- pathShas
                 , p `notElem` concatMap (map path) (Map.elems dirMap)
                 ]

    mergedMap = foldr (\(dir, e) acc -> Map.insertWith (++) dir [e] acc) dirMap newEntries

    rebuildDir newShas dir = do
        let entries             = Map.findWithDefault [] dir mergedMap
            updated             = map (updateTreeEntry pathShas) entries
            updatedWithSubtrees = map (updateSubtree newShas) updated
            treeInput           = unlines $ map formatTreeEntry updatedWithSubtrees
        newSha <- mkTree treeInput
        return $ Map.insert dir newSha newShas


parseTreeEntry :: String -> TreeEntry
parseTreeEntry line =
    let (meta, tabPath) = break (== '\t') line
        [m, t, s]       = words meta
    in TreeEntry m t (Sha s) (Path (drop 1 tabPath))


updateTreeEntry :: [(Path, Sha)] -> TreeEntry -> TreeEntry
updateTreeEntry pathShas entry =
    case lookup (path entry) pathShas of
        Just newSha -> entry { sha = newSha }
        Nothing     -> entry


formatTreeEntry :: TreeEntry -> String
formatTreeEntry e = mode e ++ " " ++ entryType e ++ " " ++ unSha (sha e) ++ "\t" ++ takeFileName (unPath (path e))


mkTree :: String -> IO Sha
mkTree treeInput = Sha . head . lines <$> runGit ["mktree"] treeInput


-- TODO: Current default commit message is "." (my convention), can open to user input
createCommit :: Sha -> IO Sha
createCommit (Sha treeSha) = Sha . head . lines <$> runGit ["commit-tree", treeSha, "-p", "origin/master", "-m", "."] ""


createBranch :: Branch -> Sha -> IO ()
createBranch (Branch branch) (Sha commitSha) =
    () <$ runGit ["update-ref", "refs/heads/" ++ branch, commitSha] ""


push :: Branch -> IO ()
push (Branch branch) = () <$ runGit ["push", "origin", branch] ""


restoreFiles :: [Path] -> [TreeEntry] -> IO ()
restoreFiles paths tree = do
    let existingPaths             = map path tree
    let (newFiles, modifiedFiles) = partition (`notElem` existingPaths) paths
    mapM_ (removeFile . unPath) newFiles
    when (not $ null modifiedFiles) $
        () <$ runGit (["checkout", "HEAD", "--"] ++ map unPath modifiedFiles) ""
