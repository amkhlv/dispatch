
module Foundation where

import Yesod
import Database.Persist.Sql (ConnectionPool)
import GHC.Generics
import Data.Text
import Network.HTTP.Conduit (Manager)
import qualified System.IO as SIO

data App = App
    { serverProto :: String
    , serverSite :: String
    , serverPort :: Int
    , serverURLPath :: String
    , clientId :: Text
    , clientSecret :: Text
    , workingDir :: FilePath
    , httpManager :: Manager
    , appConnPool    :: ConnectionPool 
    , logFileHandle :: SIO.Handle
    , users :: [String]
    }

