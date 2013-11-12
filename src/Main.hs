{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Main where

import           Control.Exception
import           Control.Monad
import           Itunes.Import
import qualified Itunes.Media       as Media
import           System.Directory   (canonicalizePath, removeFile)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

-- | Enumerates the possible parsed values of the program arguments.
data Args = Add [FilePath] | Copy [FilePath] | Help | Invalid | Unknown String
          deriving Show

main :: IO ()
main = getArgs >>= execute . parseArgs
  where
    parseArgs ("add":xs)  | (not . null) xs = Add xs
    parseArgs ("copy":xs) | (not . null) xs = Copy xs
    parseArgs ("help":_)  = Help
    parseArgs (cmd:_)     = Unknown cmd
    parseArgs _           = Invalid

-- | Print program usage to stdout.
showUsage :: IO ()
showUsage =
  putStrLn $ unlines
  [ "Usage:"
  , "  add [items...]   Add files or folders to the iTunes library, removing originals."
  , "  copy [items...]  Add files or folders to the iTunes library, preserving originals."
  , "  help             Show usage" ]

-- | Run the program as specified by the program arguments.
execute :: Args -> IO ()
execute Help =
  putStrLn "itunes: Commands for working with iTunes" >> showUsage
execute Invalid =
  putStrLn "Invalid usage." >> showUsage >> exitFailure
execute (Unknown cmd) =
  putStrLn ("Unrecognised command: " ++ cmd) >> showUsage >> exitFailure
execute (Copy paths) = canonicalize paths >>= addToItunes Media.Copy
execute (Add paths) = do
  ps <- canonicalize paths
  addToItunes Media.Move ps
  forM_ ps removeFile

-- | Return the canonical version of each given path.
-- | Paths that do not exist are unchanged.
canonicalize :: [FilePath] -> IO [FilePath]
canonicalize = mapM $ \p -> canonicalizePath p `catch` (\(_::IOException) -> return p)
