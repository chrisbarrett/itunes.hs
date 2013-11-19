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
addToItunes strategy xs = do
  itunes <- itunesFolder
  itunesDirExists <- doesDirectoryExist itunes
  if itunesDirExists
    then findMedia xs >>= mapM (toTask itunes) >>= mapM_ importMedia . concat
    else error $ "iTunes folder does not exist at expected path: " ++ itunes
  where
    findMedia paths = concat <$> mapM mediaFromPath paths
    toTask itunes x = importTasks strategy itunes x

    itunesFolder = do
      home <- getHomeDirectory
      return $
        home </> "Music" </> "iTunes" </> "iTunes Media"
        </> "Automatically Add to iTunes.localized"

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
getFilesInTree x | takeFileName x `elem` [".", ".."] = return []
getFilesInTree x = do
  isDir <- doesDirectoryExist x
  isFile <- doesFileExist x
  case (isDir, isFile) of
    (True, _) -> concat <$> getDirectoryFiles x
    (_, True) -> return [x]
    _         -> return []
  where
    getDirectoryFiles d = getDirectoryContents d >>= mapM (getFilesInTree . (</>) d)
