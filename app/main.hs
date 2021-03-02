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
  <$> strOption ( short 'c' <> metavar "COMM" <> help "XML common config" )
  <*> strOption ( short 'i' <> metavar "INST" <> help "XML instance config")

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Seminar
    owner String
    startDate Day
    startTime TimeOfDay
    repeatWeeks Int
    description String
    link String
    responsiblePerson String
    showToGroup Int
    showToAll Int
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
    yesodMiddleware = sslOnlyMiddleware 120 . defaultYesodMiddleware

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

data GoogleUser
    = GoogleUser
    { name :: Text
    , email :: Text
    , email_verified :: Bool
    } deriving Generic
instance FromJSON GoogleUser

type AuthID = Text

instance YesodAuth App where
  type AuthId App = Text
  getAuthId creds = case getUserResponseJSON creds of
    Left _ -> return Nothing
    Right (GoogleUser nm eml trusted) ->
      if trusted
      then do
        setSession "_GMAIL" eml
        setSession "_NAME" nm
        return $ Just eml
      else return Nothing
  loginDest _ = HomeR
  logoutDest _ = HomeR
  authPlugins x = [ oauth2GoogleScoped ["email", "profile"] (clientId x) (clientSecret x) ]
  authHttpManager = getsYesod httpManager
  maybeAuthId = lookupSession "_GMAIL"

data RejectionReason = NotLoggedIn | NotAllowed AuthID

idLookup :: Handler (Either RejectionReason AuthID)
idLookup = do
  ysd <- getYesod
  maid <- maybeAuthId
  case maid of
    Just aid -> return $
      if getAll $ mconcat [ All (aid /= pack x) | x <- Foundation.users ysd ] then Left (NotAllowed aid) else Right aid
    Nothing -> return $ Left NotLoggedIn

data NewSeminar = NewSeminar {
  sDate :: Text
  , sTime :: Text
  , rept :: Int
  , desc :: Text
  , link :: Maybe Text
  , resp :: Text
  , shgrp :: Int
  , shall :: Int
} deriving Show
mkSeminar :: Text -> NewSeminar -> Maybe Seminar
mkSeminar a (NewSeminar startDate startTime rWeeks description mln responsible sg sa) = do
  sd <- parseDay startDate
  st <- parseTimeOfDay startTime
  return $ Seminar (unpack a) sd st rWeeks (unpack description) (unpack $ fromMaybe "" mln) (unpack responsible) sg sa
seminarAForm :: Maybe NewSeminar -> String -> AForm Handler NewSeminar
seminarAForm mOld resp = NewSeminar
    <$> areq textField "YYYY-MM-DD" (sDate <$> mOld)
    <*> areq textField "hh:mm" (sTime <$> mOld)
    <*> areq intField "repeat weeks" (Just $ maybe 0 rept mOld)
    <*> areq textField "description" (desc <$> mOld)
    <*> aopt textField "maybe link"  (link <$> mOld)
    <*> pure (pack resp)
    <*> areq (radioField $ optionsPairs [("full info" :: Text, 2), ("as busy" :: Text, 1), ("hide" :: Text, 0)]) "show to group" (Just 2)
    <*> areq (radioField $ optionsPairs [("full info" :: Text, 2), ("as busy" :: Text, 1), ("hide" :: Text, 0)]) "show to all" (Just 2)
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
  , shgrp = seminarShowToGroup s
  , shall = seminarShowToAll s
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
  (sgRes, sgView) <- mreq (radioField $ optionsPairs [("full info" :: Text, 2), ("as busy" :: Text, 1), ("hide" :: Text, 0)]) "show to group" (shgrp <$> mOld)
  (saRes, saView) <- mreq (radioField $ optionsPairs [("full info" :: Text, 2), ("as busy" :: Text,  1), ("hide" :: Text, 0)]) "show to all" (shall <$> mOld)
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
                      <tr>
                        <td>Visibility to group
                        <td> ^{fvInput sgView}
                      <tr>
                        <td>Public visibility
                        <td> ^{fvInput saView}
                    <input type=submit value="update">
                    |]
  return (EditSem <$> iRes <*> (NewSeminar <$> dRes <*> tRes <*> wRes <*> cRes <*> lRes <*> rRes <*> sgRes <*> saRes), w)

newtype DelSem = DelSem {
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
intervalForm = renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 3) (ColSm 0) (ColSm 5)) intervalAForm

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



data ShowOption = ShowFullInfo | ShowAsBusy | Hide

whatToShow :: Maybe AuthID -> Entity Seminar -> ShowOption
whatToShow maid evt =
  case seminarShowToAll (entityVal evt) of
    2 -> ShowFullInfo
    1 -> case maid of
      Just aid ->
        if unpack aid == seminarOwner (entityVal evt)
        then ShowFullInfo
        else case seminarShowToGroup (entityVal evt) of
          2 -> ShowFullInfo
          _ -> ShowAsBusy
      Nothing -> ShowAsBusy
    _ -> case maid of
      Just aid -> 
        if unpack aid == seminarOwner (entityVal evt)
        then ShowFullInfo
        else case seminarShowToGroup (entityVal evt) of
          2 -> ShowFullInfo
          1 -> ShowAsBusy
          _ -> Hide
      Nothing -> Hide


descriptionWidget :: Maybe AuthID -> Entity Seminar -> WidgetFor App ()
descriptionWidget maid evt = case whatToShow maid evt of
  ShowAsBusy -> case maid of
    Nothing -> [whamlet|
                       <span .tag_showasbusy>
                       <span .showasbusy> busy
                       |]
    Just aid ->
      if seminarOwner (entityVal evt) == unpack aid
      then [whamlet|
                   <span .tag_showasbusy>
                   <span .showasbusy> #{seminarDescription (entityVal evt)}
                   |]
      else [whamlet|
                   <span .tag_showasbusy>
                   <span .showasbusy> busy
                   |]
  ShowFullInfo -> case maid of
    Nothing  -> [whamlet|
                        <span .tag_showfullinfo_noauth>
                        <span .showfullinfo_noauth> #{seminarDescription (entityVal evt)}
                        |]
    Just aid ->
      let ownerFlag =
            if seminarOwner (entityVal evt) == unpack aid
            then [whamlet|<span .tag_owner>|]
            else [whamlet|<span .tag_notme>|]
      in
        case seminarShowToAll (entityVal evt) of
          2 -> [whamlet|
                       ^{ownerFlag}
                       <span .tag_showfullinfo_auth>
                       <span .showfullinfo> #{seminarDescription (entityVal evt)}
                       |]
          1 -> case seminarShowToGroup (entityVal evt) of
            2 -> [whamlet|
                         ^{ownerFlag}
                         <span .tag_busy_full>
                         <span .busy_full> #{seminarDescription (entityVal evt)}
                         |]
            1 -> [whamlet|
                         ^{ownerFlag}
                         <span .tag_busy_busy>
                         <span .busy_busy> #{seminarDescription (entityVal evt)}
                         |]
            _ -> [whamlet|
                         ^{ownerFlag}
                         <span .tag_busy_hide> #{seminarDescription (entityVal evt)}
                         |]
          _ -> case seminarShowToGroup (entityVal evt) of
            2 -> [whamlet|
                         ^{ownerFlag}
                         <span .tag_hide_full>
                         <span .hide_full> #{seminarDescription (entityVal evt)}
                         |]
            1 -> [whamlet|
                         ^{ownerFlag}
                         <span .tag_hide_busy>
                         <span .hide_busy> #{seminarDescription (entityVal evt)}
                         |]
            _ -> [whamlet|
                         ^{ownerFlag}
                         <span .tag_hide_hide>
                         <span .hide_hide> #{seminarDescription (entityVal evt)}
                         |]

linkWidget :: Maybe AuthID -> Entity Seminar -> WidgetFor App ()
linkWidget maid evt =
  let semlink =
        if seminarLink (entityVal evt) == "" then [whamlet||] else [whamlet|<a href=#{seminarLink $ entityVal evt}> link|]
  in
    case whatToShow maid evt of
      ShowAsBusy -> [whamlet| --- |]
      ShowFullInfo -> case maid of
        Nothing -> semlink
        Just aid -> if seminarOwner (entityVal evt) == unpack aid
          then semlink
          else
          case seminarShowToAll (entityVal evt) of
            2 -> semlink
            1 -> case seminarShowToGroup (entityVal evt) of
              2 -> semlink
              1 -> [whamlet| --- |]
              0 -> [whamlet| --- |]
            0 -> case seminarShowToGroup (entityVal evt) of
              2 -> semlink
              1 -> [whamlet| --- |]

getSeminarsInInterval :: Maybe AuthID -> [Entity Seminar] -> (Day, Day) -> [(Day, [Entity Seminar])]
getSeminarsInInterval maid es (s,e) = [ let f = flip Prelude.filter in
  (d,
   f es $ \evt -> let  d' = (seminarStartDate $ entityVal evt)
                       w' = toInteger (seminarRepeatWeeks $ entityVal evt)
                  in  d >= d'
                      && diffDays d d' <= 7 * w'
                      && dayOfWeek d' == dayOfWeek d
                      && case whatToShow maid evt of
                           Hide -> False
                           _ -> True
  )
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

data UserType = Me | GroupMember | Public

getHomeR :: Handler Html
getHomeR = do
  mnm <- lookupSession "_NAME"
  ((requestedPeriod, widget), periodEnc) <- runFormGet intervalForm
  (pForm, pEncType) <- generateFormGet' intervalForm
  (nForm, nEncType) <- generateFormPost (seminarForm Nothing $ unpack $ fromMaybe "UNKNOWN" mnm)
  dayNow <- liftIO (utctDay <$> getCurrentTime)
  ysd <- getYesod
  eaid <- idLookup
  let maid = case eaid of { Left _ -> Nothing ; Right aid -> Just aid }
  allSems <- runDB $ selectList [] [Asc SeminarStartTime]
  let intrvl = getIntervalM dayNow requestedPeriod
  let (s,e) = fromMaybe (dayNow, addDays 2 dayNow) intrvl
  let matchingSeminars = getSeminarsInInterval maid allSems (s,e)
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
                $forall (d, evEs) <- matchingSeminars
                  $forall evE <- evEs
                    <tr>
                      <td .td_date> #{show d}
                      <td .td_time> #{show $ seminarStartTime $ entityVal evE}
                      <td .td_desc> ^{descriptionWidget maid evE}
                      <td .td_link> ^{linkWidget maid evE}
                      <td .td_resp> #{seminarResponsiblePerson $ entityVal evE}
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
  case eaid of
    Left NotLoggedIn -> defaultLayout $ do
      stylesheet
      toWidgetHead [julius|#{rawJS ourjs}|]
      [whamlet|
          <p #p_header>
          Enter time interval:
          <span #tag_timeinterval>
          <form method=get action=@{HomeR} enctype=#{pEncType} class="form-horizontal">
            ^{pForm}
          ^{semList}
          <a href=@{AuthR LoginR} style="font-size:28pt">login
        |]
    Left (NotAllowed aid) -> defaultLayout $ do
      stylesheet
      toWidgetHead [julius|#{rawJS ourjs}|]
      [whamlet|
          <p #p_header>
          <h2> User #{aid} is not on our list
          Enter time interval:
          <span #tag_timeinterval>
          <form method=get action=@{HomeR} enctype=#{pEncType} class="form-horizontal">
            ^{pForm}
          ^{semList}
          <a href=@{AuthR LoginR} style="font-size:28pt">login
        |]
    Right aid | getAll (mconcat [ All (aid /= pack x) | x <- Foundation.users ysd ]) ->
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
                    <span #tag_timeinterval>
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
                    <span #tag_timeinterval>
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
          eaid <- idLookup
          case eaid of
            Left (NotAllowed aid) -> defaultLayout (notAuthorizedPage aid)
            Left NotLoggedIn -> defaultLayout noLoginPage
            Right aid ->
              case mkSeminar aid newSem  of
                Just ns -> do
                  runDB (insert ns)
                  redirect HomeR
                Nothing -> defaultLayout $ errorPage ("DATE or TIME PARSE ERROR" :: String)
        _ -> defaultLayout $ errorPage ("Something wrong with the input. Are you logged in?" :: String)

postDelR :: Handler Html
postDelR = do
  eaid <- idLookup
  ((result, widget), enctype) <- runFormPost (keyForm (-1) "UNUSED")
  case eaid of
    Right aid ->
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
    Left NotLoggedIn -> defaultLayout noLoginPage
    Left (NotAllowed aid) -> defaultLayout $ notAuthorizedPage aid

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
        Nothing -> defaultLayout noLoginPage

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
