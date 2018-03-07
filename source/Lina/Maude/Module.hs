module Lina.Maude.Module(
  module System.FilePath,
  module System.Environment,
  get_parent_dir,
  get_MAUDE_LIB,
  get_maude_mod_path,
  get_maude_mod_absolute_path) where

import System.FilePath
import System.Environment

get_parent_dir :: FilePath -> FilePath
get_parent_dir f = head $ dirs
  where
    dirs = reverse $ splitPath f

get_MAUDE_LIB :: IO String
get_MAUDE_LIB = getEnv "MAUDE_LIB"

get_maude_mod_path :: [FilePath] -> Maybe FilePath
get_maude_mod_path dirs = lookup "maude-modules" parents                          
 where
   parents = zip (map (get_parent_dir.dropTrailingPathSeparator) dirs) dirs

get_maude_mod_absolute_path :: FilePath -> IO FilePath
get_maude_mod_absolute_path file = do
  maude_lib <- get_MAUDE_LIB
  let paths = splitSearchPath maude_lib
  case get_maude_mod_path paths of
    Just dir -> return $ dir </> file
    Nothing -> error "maude-modules path not set in MAUDE_LIB environment variable"
