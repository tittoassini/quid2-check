{-# LANGUAGE ScopedTypeVariables ,TupleSections ,GADTs ,NoMonomorphismRestriction ,OverloadedStrings #-}

import System.Environment(getArgs)
import System.Directory
import System.FilePath
import System.Process
import System.Locale
import System.ShQQ
-- import Quid2.Util.Log
import System.Log.Logger
import System.Log
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar
import Data.String
import Distribution.Simple.Utils()
import Distribution.Verbosity
import Control.Monad
import Network.HTTP
import Control.Applicative
import Control.Exception
import Control.Arrow
import Data.Maybe
import System.IO
import Data.List
import qualified Data.Text as T
-- import Data.DateTime
-- import Network.SMTPS.Gmail
--import Network.Mail.Client.Gmail
-- import Network.Mail.Mime
import Quid2.Util.Time(timeOut,secs)
import Quid2.Util.String(between)
import Quid2.Util.Email(email,titto)

t = main

x = checkServices [AService $ PortService "quid2.mooo.com" 22]

-- TODO: add sound warning.
-- BUG: does not warn if backup or git fails (TO CHK: actually cron sends a message? that I ignore).

main = do
  updateGlobalLogger rootLoggerName  (setLevel DEBUG)
  
  args <- getArgs    
  dbg $ "args " ++ show args
  case args !! 0 of
    "check" -> do 
      let cmd = if length args == 1 then "" else args !! 1
      case cmd of
        "" -> checkCmd False
        "andReport" -> checkCmd True
        _ -> error $ "Unknown command: " ++ cmd 
      
    -- "update" -> update

    backupDir -> do
    let cmd = if length args == 1 then "backup" else args !! 1
    case cmd of          
      "backup" -> backup backupDir 
      "backupLocal" -> backupLocal backupDir    
      "bigBackup" -> bigBackup backupDir
      _ -> error $ "Unknown command: " ++ cmd 

checkCmd sendOK = do
  rs <- checkServices ss
  if length rs == 0
    then when sendOK $ sendReport $ "OK. All services are up." 
    else sendReport $ unwords ["PROBLEM.",concatMap (\(s,Just r) -> unwords ["\n",s,"->",r]) rs] 
  
ss = [AService Quid2Service
     ,AService FinanceService
     ,AService $ WebService "quid2.org web" "http://quid2.org/js/quid2/ui/Tabs.js" "quid2.ui.Tab"
     ,AService $ WebService "jslib.quicquid.org" "http://jslib.quicquid.org/Request.js" "Request.prototype.getURL"
     ,AService $ WebService "kamus.it" "http://kamus.it" "Assini's Family"
     ,AService $ WebService "massimoassini.quicquid.org" "http://massimoassini.quicquid.org" "figura femminile"
     ,AService $ WebService "ska.quicquid.org" "http://ska.quicquid.org/bottom.html" "PDF"
     --,AService $ PortService "quid2.mooo.com" 2
     ,AService $ PortService "quid2.com" 22
     ,AService $ PortService "nano.quid2.com" 22      
     ]

-- Send error or ok message via Google Cloud Messaging for Android: requires a client side app.
-- or gmail with desktop notification (works in android?)
-- or a page displayed on my browser (that might be on my phone that will show a pop up when there are problems

sendReport = email titto "Quid2 Report"

class Service s where
  name :: s -> String        -- Service name
  check :: s -> IO (Maybe String) -- Nothing indicates that check passed, Maybe returns error 

data AService = forall a. Service a => AService a

instance Service AService where
   name (AService s) = name s
   check (AService s) = check s

data WebService = WebService {nameW::String,http::String,expected::String}

instance Service WebService where
  name = nameW
  
  check s = urlHas (unwords ["Could not find ",expected s,"at",http s]) (http s) (expected s) 

data FinanceService = FinanceService

instance Service FinanceService where
  name _ = "finance.quicquid.org"
  
  check _ = do
    msum <$> sequence (map chk ["ca256","sp500","us"])
      where
        chk n = do
          now <- utctDay <$> getCurrentTime 
          let url = concat ["http://finance.quicquid.org/data/events_data_",n,".js"]  
          maybeContent <- getURL 45 url
          return $ case maybeContent of
            Left (err::SomeException) -> Just $ unwords ["Could not access url",url,"got",show err]
            Right c ->
              maybe (Just $ unwords ["Data for",n,"dataset missing."]) (\d -> if diffDays now d <= 4 then Nothing else Just $ unwords ["Data for",n,"dataset has not been updated."]) (between "date:'" "'" c >>= parseYYMMDD)
                
data Quid2Service = Quid2Service

instance Service Quid2Service where
  name _ = "quid2 service"
  
  check _ = do
    m1 <- chk "http://quid2.org/api/send" "Method \"GET\" not supported" 
    m2 <- chk "http://quid2.org/api/identity" "http://quid2.org/eval" 
    return $ mplus m1 m2
      where chk = urlHas "API not working" 

data PortService = PortService {sHost::String,sPort::Int} deriving Show

instance Service PortService where
  name = show
  
  check h = do
    let p = show (sPort h)
    portOpen <- isPortOpen p <$> (readShell $ concat ["nmap -p",p," -Pn ",sHost h])
    return $ if portOpen then Nothing else Just ("DOWN: " ++ show h)

y = check (PortService "quid2.mooo.com" 22)
yy = check (PortService "x.om" 22)

isPortOpen :: String -> String -> Bool
isPortOpen p = (== "open") . head . tail . words . head . filter (isPrefixOf $ p ++ "/tcp") . lines

urlHas errMsg url expected  = do
  t1 <- getURL 30 url
  return $ case t1 of
      Left err -> Just $ unwords ["Could not access url",url,"got",show err]
      Right c  -> if isInfixOf expected c
                  then Nothing
                  else Just $ errMsg

parseYYMMDD :: String -> Maybe Day
parseYYMMDD = parseTime defaultTimeLocale ("%Y-%m-%e")

checkServices ss = filter (\(n,mv) -> isJust mv) <$> mapM (\s -> (name s,) <$> check s) ss 

getURL :: Int -> String -> IO (Either SomeException String)
getURL timeOutInSecs url = try $ timeOut (secs timeOutInSecs) $ do
      rsp <- simpleHTTP (getRequest url)
              -- fetch document and return it (as a 'String'.)
      getResponseBody rsp

{- copy over from master server (BUG: hardwired ip)
update = void $ mapM transfer ["quid2-user","quid2-store"]
     where
        transfer proj = do
          let dir = "/home/" ++ proj ++ "/." ++ proj
          runCmd ["rsync","-avz","--delete","root@94.23.66.191:"++dir++"/state",dir]
-}

bigBackup rootBackupDir = backup_ (rootBackupDir </> "big")   ["root@quid2.org:/home"]
  
backup rootBackupDir    = backup_ (rootBackupDir </> "small") ["root@quid2.org:/home/quid2-user/.quid2-user","root@quid2.org:/home/quid2-store/.quid2-store"]
          
backupLocal rootBackupDir    = backup_ (rootBackupDir </> "smallLocal") ["/home/quid2-user/.quid2-user","/home/quid2-store/.quid2-store"]

backup_ rootBackupDir sourceDirs = do
  now <- getCurrentTime 
  let time = formatTime defaultTimeLocale ("%Y-%m-%d_%H_%M_%S") now
  let backupDir = rootBackupDir </> "quid2-backup"
  createDirectoryIfMissing True backupDir         
  gitted <- doesDirectoryExist $ backupDir </> ".git"     
  when (not gitted) $ git backupDir ["init"] >> return () 
  exitCodes <- mapM (\sourceDir -> runCmd ["rsync","-avz","--exclude=debug.txt","--delete","--rsh='ssh -p2222'",sourceDir,backupDir]) sourceDirs
  dbg $ "rsync result: " ++ show exitCodes
  git backupDir ["add","-A","."]  
  -- git backupDir ["status"]  
  void $ git backupDir ["commit","-m","'"++time++"'"]
  -- git backupDir ["status"]  
  -- git backupDir ["log","--stat"]

git dir cmds = setCurrentDirectory dir >> runCmd ("git":cmds)

{-
backup2_ rootBackupDir sourceDirs = do
  now <- getCurrentTime 
  let time = formatTime defaultTimeLocale ("%Y-%m-%d_%H_%M_%S") now
  let backupDir = rootBackupDir </> "last"
  let storeDir = rootBackupDir </> time     
  createDirectoryIfMissing True backupDir   
  exitCode <- runCmd ["rsync","-avz","--delete",sourceDirs,backupDir] --  
  dbg $ "rsync result: " ++ show exitCode
  -- runCmd []
  copyDirectoryRecursiveVerbose normal backupDir storeDir
-}

runCmd cmds = do
  let cmd = unwords cmds 
  dbg cmd      
  runCommand cmd >>= waitForProcess 
  
dbg = debugM "Check"  
