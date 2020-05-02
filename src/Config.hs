{-# LANGUAGE Arrows #-}
module Config where

import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.Pickle
import Data.Maybe(fromJust)
import Data.Map

data CommonCfg = CommonCfg
  { proto :: String
  , site  :: String
  , googleId :: String
  , googleSecret :: String
  , cert :: String
  , key  :: String
  } deriving (Show, Eq)

data InstanceCfg = InstanceCfg
  { remotePort :: Int
  , urlPath :: String
  , localPort  :: Int
  , dir :: String
  , users :: [String]
  } deriving (Show, Eq)

instance XmlPickler CommonCfg where xpickle = xpCommonCfg
xpCommonCfg :: PU CommonCfg
xpCommonCfg =
  xpElem "config" $
  xpWrap (\((h,u,i,s,c,k)) -> CommonCfg h u i s c k,
           \cf ->
             (proto cf, site cf, googleId cf, googleSecret cf, cert cf, key cf)) $
  xp6Tuple
  (xpElem "proto" xpText)
  (xpElem "site" xpText)
  (xpElem "GoogleClientID" xpText)
  (xpElem "GoogleClientSecret" xpText)
  (xpElem "cert" xpText)
  (xpElem "key" xpText)

instance XmlPickler InstanceCfg where xpickle = xpInstanceCfg
xpInstanceCfg :: PU InstanceCfg
xpInstanceCfg =
  xpElem "config" $
  xpWrap (\((rp, up, lp, d, u)) -> InstanceCfg rp up lp d u,
           \cf ->
             (remotePort cf, urlPath cf, localPort cf, dir cf, users cf)) $
  xp5Tuple
  (xpElem "remotePort" xpInt)
  (xpElem "urlPath" xpText)
  (xpElem "localPort" xpInt)  
  (xpElem "dir" xpText)
  (xpElem "users" $ xpList $ xpElem  "user" xpText)

getCommonConfig :: String -> IO CommonCfg
getCommonConfig x = (fromJust . head) <$> (runX $
                                           readDocument [withRemoveWS yes] x >>>
                                           getChildren >>>
                                           isElem >>^
                                           unpickleDoc xpCommonCfg)

getInstanceConfig :: String -> IO InstanceCfg
getInstanceConfig x = (fromJust . head) <$> (runX $
                                             readDocument [withRemoveWS yes] x >>>
                                             getChildren >>>
                                             isElem >>^
                                             unpickleDoc xpInstanceCfg)
