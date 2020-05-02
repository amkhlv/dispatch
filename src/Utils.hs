module Utils (
    buildPath
    , unauthorizedUserMsg
    ) where

import System.IO
import Data.Text
import Data.Either
import qualified Filesystem.Path.CurrentOS as FPOS

buildPath' :: FPOS.FilePath -> [Text] -> Text
buildPath' acc [] = fromRight (pack "/tmp/x") (FPOS.toText acc)
buildPath' acc (x:xs) = buildPath' (acc FPOS.</> FPOS.fromText x) xs

buildPath :: [Text] -> Text
buildPath xs = buildPath' FPOS.empty xs

unauthorizedUserMsg :: Text -> String
unauthorizedUserMsg x = 
  "=== Unauthorized  user ===\n>>>" ++ unpack x ++ "<<<\n" ++ "=========================="