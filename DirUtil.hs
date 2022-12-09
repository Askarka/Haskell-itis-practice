module DirUtil where

import System.Environment
import System.Directory
import Data.ByteString (count, pack, length)
import Control.Monad.Reader
import System.Directory.Internal.Prelude (for)
import System.FilePath (joinPath)
import Control.Monad.RWS (MonadState)
import Control.Monad.RWS.Class
import Control.Monad.State

parseArgs :: [String] -> (FilePath, Maybe Int)
parseArgs argList = case argList of
    [arg1, arg2] -> (arg1, Just (read arg2))
    [arg1]       -> (arg1, Nothing)
    _            -> error "Incorrect arg list!"

getDirectoryPaths :: (MonadReader (FilePath, Maybe Int) m, MonadIO m) => m [FilePath]
getDirectoryPaths = do
    args <- ask
    case args of
        (path, depth) -> getNestedDirs path depth
    

getNestedDirs :: (MonadIO m) => FilePath -> Maybe Int -> m [FilePath]
getNestedDirs path depth = do 
    isDir <- liftIO $ doesDirectoryExist path
    if isDir 
        then do
            nestedPaths <- fmap (map (\dir ->joinPath [path, dir])) (liftIO $ listDirectory path)
            let getNestedFilesForPaths d = forM nestedPaths (`getNestedDirs` d) in
                case depth of
                    Nothing -> fmap (path:) $ concat <$> getNestedFilesForPaths Nothing
                    Just 0  -> pure [path]
                    Just n  -> fmap (path:) $ concat <$> getNestedFilesForPaths (Just (n - 1))
        else pure []

getDirectoryContentSize :: FilePath -> IO Integer
getDirectoryContentSize path = do 
    isDir <- doesDirectoryExist path
    if isDir
        then do
            paths <- fmap (map (\dir ->joinPath [path, dir])) (liftIO $ listDirectory path)
            sum <$> for paths getDirectoryContentSize
        else getFileSize path


getDirectoryFileCounts :: (MonadIO m) => [FilePath] -> m [Int]
getDirectoryFileCounts paths = forM paths $ \path -> liftIO $ fmap Prelude.length (listDirectory path)

getDirectoryFileSizes :: (MonadIO m) => [FilePath] -> m [Integer]
getDirectoryFileSizes paths = forM paths (\path -> liftIO $ sum <$> (listDirectory path >>= mapM (getDirectoryContentSize . (\dir ->joinPath [path, dir]))))

getDirectoryInfo :: (MonadState [FilePath] m, MonadIO m) => m [(FilePath, Int, Integer)]
getDirectoryInfo = do
    paths <- get
    fileCounts <- getDirectoryFileCounts paths
    fileSizes <- getDirectoryFileSizes paths
    return $ zip3 paths fileCounts fileSizes

printDirectoryInfo :: (MonadIO m) => [(FilePath, Int, Integer)] -> m()
printDirectoryInfo infos = do
    liftIO $ putStrLn "Counts: "
    forM_ infos (\(path, fileCount, _) -> liftIO $ putStrLn (path ++ " " ++ show fileCount))
    liftIO $ putStrLn "Sizes: "
    forM_ infos (\(path, _, fileSizes) -> liftIO $ putStrLn (path ++ " " ++ show fileSizes))
    

main :: IO ()
main = do
    args <- getArgs
    files <- runReaderT getDirectoryPaths (parseArgs args)
    filesInfo <- evalStateT getDirectoryInfo files
    printDirectoryInfo filesInfo