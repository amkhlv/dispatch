{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

import Yesod
import Yesod.Core
import Yesod.Auth
import Yesod.Auth.OAuth2 (getUserResponseJSON)
import Yesod.Auth.OAuth2.Google
import Yesod.Form.Bootstrap3
import qualified System.IO as SIO
import Options.Applicative
import Data.Semigroup ((<>))
import Network.HTTP.Client.Conduit (Manager, newManager)
import Data.Text
import Data.Int
import qualified Data.Set as S
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Monoid
import Data.Maybe
import Text.Printf
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Database.Persist
import Database.Persist.Sql (SqlBackend, SqlPersistT, fromSqlKey, toSqlKey)
import Database.Persist.Sqlite  (createSqlitePool, runSqlPool, sqlDatabase, sqlPoolSize, runMigration)
import Database.Persist.TH
import Control.Applicative  ((<$>), (<*>))
import Yesod.Form.Jquery
import Control.Monad.Logger (runStdoutLoggingT)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base64 as B64
import System.IO
import qualified Filesystem.Path as FP
import qualified Filesystem.Path.CurrentOS as FPOS
import Data.Either
import GHC.Generics
import Text.Julius
import qualified Text.Blaze as TB
import qualified Config as CONF
import Foundation
import Utils

data Clops = Clops { xmlCommon :: String , xmlInstance :: String }

cloparser :: Parser Clops
cloparser = Clops
  <$> (strOption ( short 'c' <> metavar "COMM" <> help "XML common config" ))
  <*> (strOption ( short 'i' <> metavar "INST" <> help "XML instance config"))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Seminar
    owner String
    startDate Day
    startTime TimeOfDay
    repeatWeeks Int
    description String
    link String
    responsiblePerson String
    deriving Show
|]

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

mkYesod "App" [parseRoutes| 
/list HomeR GET
/newseminar NewR POST
/delseminar DelR POST
/editseminar EditR GET POST
/auth AuthR Auth getAuth
|]

-- https://hackage.haskell.org/package/yesod-core-1.6.17.1/docs/Yesod-Core.html
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootMaster $ \x ->
        pack $ serverProto x ++ "://" ++ serverSite x ++ ":" ++ show (serverPort x) ++ serverURLPath x

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes

    makeSessionBackend x = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        (unpack $ buildPath [pack $ workingDir x, pack "client_session_key.aes"])
    yesodMiddleware = (sslOnlyMiddleware 120) . defaultYesodMiddleware

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

data GoogleUser
    = GoogleUser
    { name :: Text
    , email :: Text
    } deriving Generic
instance FromJSON GoogleUser

instance YesodAuth App where
  type AuthId App = Text
  getAuthId creds = case getUserResponseJSON creds of
    Left _ -> return Nothing
    Right (GoogleUser nm eml) -> do
      setSession "_GMAIL" eml
      setSession "_NAME" nm
      return $ Just eml
  loginDest _ = HomeR
  logoutDest _ = HomeR
  authPlugins x = [ oauth2GoogleScoped ["email", "profile"] (clientId x) (clientSecret x) ]
  authHttpManager = getsYesod httpManager
  maybeAuthId = lookupSession "_GMAIL"

data NewSeminar = NewSeminar {
  sDate :: Text
  , sTime :: Text
  , rept :: Int
  , desc :: Text
  , link :: Maybe Text
  , resp :: Text
} deriving Show
mkSeminar :: Text -> NewSeminar -> Maybe Seminar
mkSeminar a (NewSeminar startDate startTime rWeeks description mln responsible) = do
  sd <- parseDay startDate
  st <- parseTimeOfDay startTime
  return $ Seminar (unpack a) sd st rWeeks (unpack description) (unpack $ fromMaybe "" mln) (unpack responsible)
seminarAForm :: Maybe NewSeminar -> String -> AForm Handler NewSeminar
seminarAForm mOld resp = NewSeminar
    <$> areq textField "YYYY-MM-DD" (sDate <$> mOld)
    <*> areq textField "hh:mm" (sTime <$> mOld)
    <*> areq intField "repeat weeks" (Just $ maybe 0 rept mOld)
    <*> areq textField "description" (desc <$> mOld)
    <*> aopt textField "maybe link"  (link <$> mOld)
    <*> pure (pack resp)
    <*  bootstrapSubmit ("Register" :: BootstrapSubmit Text)
seminarForm m  = renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 2) (ColSm 0) (ColSm 2)) . seminarAForm m
formatSeminar :: Seminar -> NewSeminar
formatSeminar s = NewSeminar {
  sDate = pack . showGregorian $ seminarStartDate s
  , sTime = pack . (\x -> printf "%02d" (todHour x) ++ ":" ++ printf "%02d" (todMin x)) $ seminarStartTime s
  , rept = seminarRepeatWeeks s
  , desc = pack $ seminarDescription s
  , link = let sl = seminarLink s in if sl == "" then Nothing else Just $ pack sl
  , resp = pack $ seminarOwner s
}
data EditSem = EditSem {
  editId :: Int64 
  , modifiedSeminar  :: NewSeminar
} deriving Show
editForm :: Int64 -> String -> Maybe NewSeminar -> Html -> MForm Handler (FormResult EditSem, Widget)
editForm i resp mOld extra = do 
  (iRes, iView) <- mreq hiddenField "UNUSED" (Just i)
  (dRes, dView) <- mreq textField "YYYY-MM-DD" (sDate <$> mOld)
  (tRes, tView) <- mreq textField "hh:mm" (sTime <$> mOld)
  (wRes, wView) <- mreq intField "repeat weeks" (Just $ maybe 0 rept mOld)
  (cRes, cView) <- mreq textField "description" (desc <$> mOld)
  (lRes, lView) <- mopt textField "maybe link"  (link <$> mOld)
  (rRes, rView) <- mreq hiddenField "UNUSED" (Just $ pack resp)
  let w = toWidget  
            [whamlet|
                    #{extra}
                    ^{fvInput iView}
                    ^{fvInput rView}
                    <table>
                      <tr>
                        <td>Date
                        <td> ^{fvInput dView}
                      <tr>
                        <td>Time
                        <td> ^{fvInput tView}
                      <tr>
                        <td>Repeat weeks
                        <td> ^{fvInput wView}
                      <tr>
                        <td>Description
                        <td> ^{fvInput cView}
                      <tr>
                        <td>Maybe link
                        <td> ^{fvInput lView}
                    <input type=submit value="update">
                    |]
  return (EditSem <$> iRes <*> (NewSeminar <$> dRes <*> tRes <*> wRes <*> cRes <*> lRes <*> rRes), w)

data DelSem = DelSem {
  delId :: Int64
} deriving Show
keyForm :: Int64 -> Text -> Html -> MForm Handler (FormResult DelSem, Widget)
keyForm i t extra = do
  (iRes, iView) <- mreq hiddenField "UNUSED" (Just i)
  let w = toWidget
            [whamlet|
                    #{extra}
                    ^{fvInput iView}
                    <input type=submit value=#{t}>
                    |]
  return (DelSem <$> iRes, w)

data Interval = Interval {
  periodStart :: Text
  , periodEnd :: Text
} deriving Show
intervalAForm :: AForm Handler Interval
intervalAForm = Interval
  <$> areq textField "period starting: YYYY-MM-DD" Nothing
  <*> areq textField "period ending: YYYY-MM-DD" Nothing
  <*  bootstrapSubmit ("List" :: BootstrapSubmit Text)
intervalForm = renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 2) (ColSm 0) (ColSm 6)) intervalAForm

parseDay :: Text -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%F" . unpack

parseTimeOfDay :: Text -> Maybe TimeOfDay
parseTimeOfDay = parseTimeM True defaultTimeLocale "%R" . unpack

getIntervalM :: Day -> FormResult Interval -> Maybe (Day, Day)
getIntervalM nd (FormSuccess interval) =
  case (parseDay $ periodStart interval, parseDay $ periodEnd interval) of
    (Just x, Just y) -> Just (x,y)
    _ -> Nothing
getIntervalM nd FormMissing = Just (nd, addDays 2 nd)
getIntervalM nd (FormFailure _) = Nothing

getSeminarsInInterval :: [Entity Seminar] -> (Day, Day) -> [(Day, [Entity Seminar])]
getSeminarsInInterval es (s,e) = [ let f = flip Prelude.filter in
  (d, f es $ \evt -> let  d' = (seminarStartDate $ entityVal evt)
                          w' = toInteger (seminarRepeatWeeks $ entityVal evt)
                      in  d >= d' && diffDays d d' <= 7 * w' && dayOfWeek d' == dayOfWeek d )
  | d <- [s..e]]

collectIds :: [(Day, [Entity Seminar])] -> [Int64]
collectIds = collectIds' S.empty
collectIds' acc [] = S.toList acc
collectIds' acc ((_, es):rest) =
  let newids = S.fromList [ fromSqlKey (entityKey e) | e <- es ]
  in collectIds' (S.union acc newids) rest

lookupKeyForm' i ps = case  [p | p <- ps, fst p == i] of
  [] -> Nothing
  x:xs -> Just $ snd x
lookupKeyForm e ps = let i = fromSqlKey (entityKey e) in lookupKeyForm' i ps

errorPage :: TB.ToMarkup a => a -> WidgetFor App ()
errorPage msg = [whamlet|
          #{msg}
          <hr>
          <a href=@{HomeR}> return to main page
          |]

notAuthorizedPage u = [whamlet|
    User #{u} is not authorized to do that
    <hr>
    <a href=@{AuthR LoginR}>login
    <br>
    <a href=@{HomeR}>main page
  |]

noLoginPage = [whamlet|
    To perform this action you need to
    <a href=@{AuthR LoginR}>log in
    <br>
    <a href=@{HomeR}>main page
  |]

stylesheet = addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"

getHomeR :: Handler Html
getHomeR = do
  mnm <- lookupSession "_NAME"
  ((requestedPeriod, widget), periodEnc) <- runFormGet intervalForm
  (pForm, pEncType) <- generateFormGet' intervalForm
  (nForm, nEncType) <- generateFormPost (seminarForm Nothing $ unpack $ fromMaybe "UNKNOWN" mnm)
  dayNow <- liftIO (utctDay <$> getCurrentTime)
  ysd <- getYesod
  maid <- maybeAuthId
  allSems <- runDB $ selectList [] [Asc SeminarStartTime]
  let intrvl = getIntervalM dayNow requestedPeriod
  let (s,e) = fromMaybe (dayNow, addDays 2 dayNow) intrvl
  let matchingSeminars = getSeminarsInInterval allSems (s,e)
  delPairs <- sequence [
      (\f -> (i,f)) <$> generateFormPost (keyForm i "delete")
      | i <- collectIds $ matchingSeminars
    ]
  editPairs <- sequence [
      (\f -> (i,f)) <$> generateFormGet' (keyForm i "edit")
      | i <- collectIds $ matchingSeminars
    ]
  ourjs <- liftIO $ readFile (unpack $ buildPath [pack $ workingDir ysd, "dispatch.js"])
  let semList = case intrvl of
        Just (s,e) ->
          [whamlet|
              Seminars found for period from #{show s} to #{show e}:
              <table .table>
                <tr>
                  <th #th_date> date 
                  <th #th_time> time 
                  <th #th_desc> description 
                  <th #th_link> maybe link
                  <th #th_resp> added by
                  <th #th_delete> delete
                $forall (d, evEs) <- getSeminarsInInterval allSems (s,e)
                  $forall evE <- evEs
                    <tr>
                      <td .td_date> #{show d}
                      <td .td_time> #{show $ seminarStartTime $ entityVal evE}
                      <td .td_desc> #{pack $ seminarDescription $ entityVal evE}
                      $if (seminarLink $ entityVal evE) == ""
                        <td .td_link> no link 
                      $else 
                        <td .td_link>
                          <a href=#{seminarLink $ entityVal evE}> link
                      <td .td_resp> #{pack $ seminarResponsiblePerson $ entityVal evE}
                      $maybe uid <- maid
                        $if uid == (pack $ seminarOwner $ entityVal evE)
                          <td>  
                                $maybe df <- lookupKeyForm evE delPairs
                                  <form method=post action=@{DelR} enctype=#{snd df}>
                                    ^{fst df}
                                $nothing 
                                  <p> could not lookup ...
                                $maybe df <- lookupKeyForm evE editPairs
                                  <form method=get action=@{EditR} enctype=#{snd df}>
                                    ^{fst df}
                                $nothing 
                                  <p> could not lookup ...
                        $else 
                          <td> not you
                      $nothing 
                        <td> not logged in
          |]
        Nothing -> errorPage ("there was an error in parsing time interval" :: String)
  case maid of
    Nothing -> defaultLayout $ do
      stylesheet
      toWidgetHead [julius|#{rawJS ourjs}|]
      [whamlet|
          <p #p_header>
          Enter time interval:
          <form method=get action=@{HomeR} enctype=#{pEncType} class="form-horizontal">
            ^{pForm}
          ^{semList}
          <a href=@{AuthR LoginR} style="font-size:28pt">login
        |]
    Just aid | getAll (mconcat [ All (aid /= pack x) | x <- Foundation.users ysd ]) ->
                defaultLayout $ do
                  toWidgetHead [julius|#{rawJS ourjs}|]
                  stylesheet
                  [whamlet| 
                    <p #p_header>
                    <p #p_welcome> Welcome 
                            #{fromMaybe "UNKNOWN" mnm}
                            #{show aid} 
                    <p #p_notallowed> Unfortunately, you are not allowed to submit new events. 
                                      If you do want to submit, please contact administrator.
                    Enter time interval:
                    <form method=get action=@{HomeR} enctype=#{pEncType} class="form-horizontal">
                      ^{pForm}
                    ^{semList}
                  |]
             | otherwise ->
                defaultLayout $ do
                  toWidgetHead [julius|#{rawJS ourjs}|]
                  stylesheet
                  [whamlet| 
                    <p #p_header>
                    <p #p_welcome> Welcome 
                            #{fromMaybe "UNKNOWN" mnm}
                            #{show aid} 
                    Enter time interval:
                    <form method=get action=@{HomeR} enctype=#{pEncType} class="form-horizontal">
                      ^{pForm}
                    ^{semList}
                    <hr>
                    You can submit new event:
                    <form method=post action=@{NewR} enctype=#{nEncType} class="form-horizontal">
                      ^{nForm}
                    <p #p_footer>
                  |]

postNewR :: Handler Html
postNewR = do
  mnm <- lookupSession "_NAME"
  ((result, widget), enctype) <- runFormPost (seminarForm Nothing $ unpack $ fromMaybe "UNKNOWN" mnm)
  ysd <- getYesod
  case result of
        FormSuccess newSem -> do
          maid <- maybeAuthId
          case maid of
            Just aid | getAll (mconcat [ All (aid /= pack x) | x <- Foundation.users ysd ]) ->
                        liftIO (
                          putStrLn (unauthorizedUserMsg aid) >>
                          SIO.hPutStrLn (logFileHandle ysd) (unauthorizedUserMsg aid) >>
                          SIO.hFlush (logFileHandle ysd)
                          ) >> defaultLayout (notAuthorizedPage aid)
                     | otherwise -> case mkSeminar aid newSem  of
                        Just ns -> do
                          runDB (insert ns)
                          redirect HomeR
                        Nothing -> defaultLayout $ errorPage ("DATE or TIME PARSE ERROR" :: String)
            _ -> defaultLayout $ errorPage ("Invalide input, please try again" :: String)

postDelR :: Handler Html
postDelR = do
  maid <- maybeAuthId
  ((result, widget), enctype) <- runFormPost (keyForm (-1) "UNUSED")
  case maid of
    Just aid ->
      case result of
        FormSuccess ds -> do
          let k = toSqlKey $ delId ds
          sm <- runDB $ get k
          case sm of
            Just s ->
              let o = seminarOwner s in
                if aid == pack o
                  then do
                    runDB (delete k)
                    redirect HomeR
                  else defaultLayout $ notAuthorizedPage aid
            Nothing -> defaultLayout $ errorPage ("there is no such record" :: String)
        _ -> defaultLayout $ errorPage("form not understood" :: String)
    Nothing -> defaultLayout noLoginPage

getEditR :: Handler Html
getEditR = do
  ((result, widget), enctype) <- runFormGet (keyForm (-1) "UNUSED")
  mnm <- lookupSession "_NAME"
  case result of
    FormSuccess ds -> do
      let k = toSqlKey $ delId ds
      sm <- runDB $ get k
      case sm of
        Just s -> do
          (eForm, eEncType) <- generateFormPost (editForm (delId ds) (unpack $ fromMaybe "UNKNOWN" mnm) (Just $ formatSeminar s))
          defaultLayout $ do
            stylesheet
            [whamlet|
                    Modify event:
                    <form method=post action=@{EditR} enctype=#{eEncType} class="form-horizontal">
                      ^{eForm}
            |]
        Nothing -> defaultLayout $ errorPage ("there is no such record" :: String)

postEditR :: Handler Html 
postEditR = do
  maid <- maybeAuthId
  ((result, widget), enctype) <- runFormPost (editForm (-1) "UNUSED" Nothing)
  case result of 
    FormSuccess modSem -> do 
      let k = toSqlKey $ editId modSem
      msem <- runDB $ get k
      case maid of 
        Just aid ->
          case msem of 
            Just sem -> 
              let o = seminarOwner sem in 
                if aid == pack o 
                  then
                    case mkSeminar aid (modifiedSeminar modSem) of 
                      Just s -> do 
                                  runDB $ Database.Persist.replace k s 
                                  redirect HomeR
                      Nothing -> defaultLayout $ errorPage ("formatting error" :: String)
                  else defaultLayout (notAuthorizedPage aid)
            Nothing -> defaultLayout $ errorPage ("internal error" :: String)
        Nothing -> defaultLayout $ noLoginPage

main :: IO ()
main = do
  clops <- execParser $
    info
    (cloparser <**> helper)
    (fullDesc <>
     progDesc "Event Scheduler" <>
     header "Event Scheduler")
  cfgComm <- CONF.getCommonConfig $ xmlCommon clops
  cfgInst <- CONF.getInstanceConfig $ xmlInstance clops
  let workdir = pack $ CONF.dir cfgInst
  log <- openFile (unpack $ buildPath [workdir, pack "server.log"]) WriteMode
  man <- newManager
  pool <- runStdoutLoggingT $ createSqlitePool (buildPath [workdir, pack "base.sqlite"]) 10
  runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
  let tls = tlsSettings (CONF.cert cfgComm) (CONF.key cfgComm)
  a <- toWaiApp (App {
                    serverProto = CONF.proto cfgComm
                    , serverSite = CONF.site cfgComm
                    , serverPort = CONF.remotePort cfgInst
                    , serverURLPath = CONF.urlPath cfgInst
                    , clientId = pack $ CONF.googleId cfgComm
                    , clientSecret = pack $ CONF.googleSecret cfgComm
                    , workingDir = unpack workdir
                    , httpManager = man
                    , appConnPool = pool
                    , logFileHandle = log
                    , users = CONF.users cfgInst
                    })
  runTLS tls (setPort (CONF.localPort cfgInst) defaultSettings) a
  hClose log
