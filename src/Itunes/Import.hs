module Itunes.Import
       (
         addToItunes
       )
       where
import           Control.Applicative
import           Control.Monad
import           Itunes.Media
import           System.Directory
import           System.FilePath.Posix

-- | Add files to iTunes with a strategy enumerated by ImportStrategy.
addToItunes :: ImportStrategy -> [FilePath] -> IO ()
addToItunes strategy xs =
  findMedia xs >>= mapM toTask >>= mapM_ importMedia . concat
  where
    findMedia paths = liftM concat $ mapM mediaFromPath paths
    itunesFolder = "~/Music/iTunes/iTunes Media/Automatically Add to iTunes.localized"
    toTask = importTasks strategy itunesFolder
    importMedia t = do
      runTask t
      putStrLn $ "  A " ++ taskName t ++ "\n"

-- | Filter the input files for importable items.
mediaFromPath :: FilePath -> IO [Importable]
mediaFromPath p = do
  isDir <- doesDirectoryExist p
  isMedia <- isMediaFile p
  isZip <- isZipFile p
  case (isDir, isMedia, isZip) of
    (True, _, _) -> liftM concat $ getFilesInTree p >>= mapM mediaFromPath
    (_, True, _) -> return [ MediaFile p ]
    (_, _, True) -> return [ ZipFile p ]
    _            -> return []

-- | Walk the directory tree to find all files below a given path.
getFilesInTree :: FilePath -> IO [FilePath]
getFilesInTree d | takeFileName d `elem` [".", ".."] = return []
getFilesInTree d = do
  isDir <- doesDirectoryExist d
  isFile <- doesFileExist d
  case (isDir, isFile) of
    (True, _) -> concat <$> (getDirectoryContents d >>= mapM (getFilesInTree . (</>) d))
    (_, True) -> return [d]
    _         -> return []
