{-|
Module      : RSS
Description : Provides structure for manipulating feeds and items
Copyright   : (c) Luc Chabassier, 2019
License     : MIT
Maintainer  : rsscli@dwarfmaster.net
Stability   : experimental
Portability : POSIX

This modules provide structure to represent feeds and items.  It also contains
the generic structure for RSS providers. RSS providers are structures that can
be used to get the RSS items of a feed, and can be serialised onto a single url
string.

-}

{-# LANGUAGE FlexibleContexts #-}

module RSS ( Feed(..), Item(..), Provider(..)
           , readFeed
           ) where

import           Prelude                   ()
import           Relude                    hiding (ByteString)
import qualified Database                  as DB
import           GHC.Generics
import qualified Text.Show
import qualified Text.Feed.Types           as Fd
import qualified Text.Feed.Import          as Imp
import qualified Text.Feed.Query           as Q
import           System.Process
import           System.Exit
import           System.IO
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Header
import qualified Data.ByteString.Lazy      as BS
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as UTF
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Maybe
import           Control.Monad.Except
import           Control.Monad.Trans
import           Control.Monad.Catch
import           Control.Monad.Catch.Pure

--   _____             _ 
--  |  ___|__  ___  __| |
--  | |_ / _ \/ _ \/ _` |
--  |  _|  __/  __/ (_| |
--  |_|  \___|\___|\__,_|
--                       

-- | Struture representing a feed
data Feed = Feed
          { fdName   :: Text     -- ^ The name of the feed
          , fdTags   :: [Text]   -- ^ The tags of the feed
          , fdUrl    :: Text     -- ^ The url of the website of the feed
          , fdRssUrl :: Provider -- ^ The provider of the feed (used to identify the feed)
          } deriving (Show,Generic)


-- | Structure representing an item
data Item = Item
          { itName    :: Text    -- ^ The name of the item
          , itFlags   :: [Char]  -- ^ The flags [a-zA-Z] of the item
          , itUrl     :: Text    -- ^ The url of the item
          , itID      :: Integer -- ^ The ID of the item in the database
          , itGUID    :: Text    -- ^ The GUID of the item
          , itAuthor  :: Text    -- ^ The author of the item
          , itDate    :: UTCTime -- ^ The time of the item
          , itContent :: Text    -- ^ The full content of the item
          , itUnread  :: Bool    -- ^ Weither the item has been read or not
          } deriving (Show,Generic)

convertFeed :: Provider -> Fd.Feed -> (Feed,[Item])
convertFeed prv feed = (resultFeed, mapMaybe mkItem $ Q.getFeedItems feed)
 where url :: Text
       url = maybe "" id $ Q.getFeedHome feed
       resultFeed :: Feed
       resultFeed = Feed (Q.getFeedTitle feed) [] url prv
       mkItem :: Fd.Item -> Maybe Item
       mkItem i = do
           title <- Q.getItemTitle i
           link <- Q.getItemLink i
           let guid = fromMaybe (False,"") $ Q.getItemId i
           let author = fromMaybe "" $ Q.getItemAuthor i
           let time = fromMaybe (UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0))
                    $ join $ Q.getItemPublishDate i
           desc <- Q.getItemDescription i
           return $ Item title
                         []
                         link
                         0
                         (if fst guid then snd guid else link)
                         author
                         time
                         desc
                         False

--   ____                 _     _               
--  |  _ \ _ __ _____   _(_) __| | ___ _ __ ___ 
--  | |_) | '__/ _ \ \ / / |/ _` |/ _ \ '__/ __|
--  |  __/| | | (_) \ V /| | (_| |  __/ |  \__ \
--  |_|   |_|  \___/ \_/ |_|\__,_|\___|_|  |___/
--                                              

-- | Provider of a feed
data Provider = FromUrl Text            -- ^ URL from an RSS feed online
              | Transform Text Provider -- ^ A command to apply on the result of another provider
              | Generator Text          -- ^ A command which output generate a RSS value
              deriving (Generic,Show)

readFeed :: Provider -> IO (Either Text (Feed,[Item]))
readFeed prv =
    let ExceptT result = readFeed' prv :: ExceptT Text IO (Feed,[Item])
     in result `catch` displayError
 where displayError :: HttpException -> IO (Either Text a)
       displayError err = return $ Left $ show err

readFeed' :: (MonadIO m, MonadError Text m, MonadCatch m)
          => Provider -> m (Feed,[Item])
readFeed' prv = do
    text <- readFeedText prv
    rss  <- liftMaybe err $ Imp.parseFeedSource $ UTF.toString text
    return $ convertFeed prv rss
 where err :: Text
       err = "Couldn't parse feed from " <> show prv
       liftMaybe :: MonadError Text m => Text -> Maybe a -> m a
       liftMaybe err = maybe (throwError err) return

readFeedText :: (MonadIO m, MonadError Text m, MonadCatch m)
             => Provider -> m ByteString
readFeedText (FromUrl url) = downloadFile url

readFeedText (Transform cmd prv) = do
    out <- readFeedText prv
    (exit,result) <- liftIO $ executeCommand cmd out
    if exit /= 0
       then throwError $ "Command \"" <> cmd <> "\" terminated with exit code " <> show exit
       else return result

readFeedText (Generator cmd) = do
    (exit,result) <- liftIO $ executeCommand cmd BS.empty
    if exit /= 0
      then throwError $ "Command \"" <> cmd <> "\" terminated with exit code " <> show exit
      else return result

-- | Takes a command and a bytestring, and execute the command, passing the bytestream as standard
-- input, returning a bytestream of the standard output and the exit code of the command.
executeCommand :: Text -> ByteString -> IO (Int,ByteString)
executeCommand cmd stdin = do
    (stdinH,stdoutH,stderrH,handle) <- createProcess command
    ret <- case (stdinH,stdoutH) of
      (Just inH, Just outH) -> do
          BS.hPut inH stdin
          hClose inH
          stdout <- BS.hGetContents outH
          exitCode <- waitForProcess handle
          return (readExitCode exitCode,stdout)
      _ -> return (127, BS.empty)
    cleanupProcess (stdinH,stdoutH,stderrH,handle)
    return ret
 where command :: CreateProcess
       command = (shell $ toString cmd) { std_in = CreatePipe, std_out = CreatePipe, std_err = NoStream }
       readExitCode :: ExitCode -> Int
       readExitCode ExitSuccess     = 0
       readExitCode (ExitFailure n) = n

-- TODO pretty print http exception
downloadFile :: forall m. (MonadIO m, MonadError Text m, MonadCatch m)
             => Text -> m ByteString
downloadFile url = downloadFile' url `catch` handleHttpException
 where handleHttpException :: HttpException -> m ByteString
       handleHttpException e = throwError $ "HTTP error on \"" <> url <> "\" : "
                            <> fromString (displayException e)

downloadFile' :: (MonadIO m, MonadError Text m, MonadCatch m)
              => Text -> m ByteString
downloadFile' url = do
    man <- liftIO $ newManager tlsManagerSettings
    req' <- case parseRequest $ toString url of
              Nothing -> throwError $ "Couldn't parse url \"" <> url <> "\""
              Just r  -> return r
    let req = req' { requestHeaders = (hUserAgent, "Liferea/1.4.14 (Linux; en_US.UTF8; http://liferea.sf.net/)") : requestHeaders req' }
    response <- liftIO $ httpLbs req man
    let status = responseStatus response
    if status == ok200
       then return $ responseBody response
       else throwError $ "Failed to download \"" <> url <> "\" : "
                      <> show (statusMessage status)
                      <> " (" <> show (statusCode status) <> ")"


--  ____        _        _                    
-- |  _ \  __ _| |_ __ _| |__   __ _ ___  ___ 
-- | | | |/ _` | __/ _` | '_ \ / _` / __|/ _ \
-- | |_| | (_| | || (_| | |_) | (_| \__ \  __/
-- |____/ \__,_|\__\__,_|_.__/ \__,_|___/\___|
--                                            

-- | Create a textual identifier for a provider
makeRssUrl :: Provider -> Text
makeRssUrl (FromUrl url)       = url
makeRssUrl (Generator cmd)     = "exec:" <> cmd
makeRssUrl (Transform cmd prv) = "filter:" <> cmd <> ":" <> makeRssUrl prv

-- | Convert a feed in a storable format
makeFeedRow :: Feed -> DB.FeedRow
makeFeedRow (Feed name tags url rss) = DB.FeedR (makeRssUrl rss) url name 0 False ""

-- | Merge a feed into a FeedRow, returns nothing if the FeedRow is unchanged
-- Assumes the RssUrl is the same
mergeFeedIntoRow :: Feed -> DB.FeedRow -> Maybe DB.FeedRow
mergeFeedIntoRow (Feed name tags url _) frow = undefined

-- | Take a provider and see if it has a feed associated in the database, otherwise creates it
getOrMakeStoredProvider :: DB.Database -> Feed -> IO DB.FeedRow
getOrMakeStoredProvider db prv = undefined



