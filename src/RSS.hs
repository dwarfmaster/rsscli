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

module RSS where

import           Prelude               ()
import           Relude
import           GHC.Generics
import qualified Text.Show
import qualified Text.Feed.Types       as Fd
import qualified Text.Feed.Import      as Imp
import qualified Text.Feed.Query       as Q
import qualified Network.Download      as Down
import           System.Process
import           System.Exit
import           System.IO
import qualified Data.ByteString       as BS
import qualified Data.ByteString.UTF8  as UTF
import           Control.Monad.Except
import           Control.Monad.Trans

--   _____             _ 
--  |  ___|__  ___  __| |
--  | |_ / _ \/ _ \/ _` |
--  |  _|  __/  __/ (_| |
--  |_|  \___|\___|\__,_|
--                       

-- | Struture representing a feed
data Feed = Feed
          { fdName :: Text      -- ^ The name of the feed
          , fdTags :: [Text]    -- ^ The tags of the feed
          , fdUrl  :: Provider  -- ^ The provider of the feed (used to identify the feed)
          } deriving (Show,Generic)


-- | Structure representing an item
data Item = Item
          { itName    :: Text    -- ^ The name of the item
          , itFlags   :: [Char]  -- ^ The flags [a-zA-Z] of the item
          , itUrl     :: Text    -- ^ The url of the item
          , itID      :: Integer -- ^ The ID of the item in the database
          , itAuthor  :: Text    -- ^ The author of the item
          , itContent :: Text    -- ^ The full content of the item
          , itUnread  :: Bool    -- ^ Weither the item has been read or not
          } deriving (Show,Generic)

convertFeed :: Provider -> Fd.Feed -> (Feed,[Item])
convertFeed = undefined

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

readFeed :: (MonadIO m, MonadError Text m)
         => Provider -> m (Feed,[Item])
readFeed prv = do
    text <- readFeedText prv
    rss  <- liftMaybe err $ Imp.parseFeedSource $ UTF.toString text
    return $ convertFeed prv rss
 where err :: Text
       err = "Couldn't parse feed from " <> show prv
       liftMaybe :: MonadError Text m => Text -> Maybe a -> m a
       liftMaybe err = maybe (throwError err) return

readFeedText :: (MonadIO m, MonadError Text m)
             => Provider -> m ByteString
readFeedText (FromUrl url) = do
    downloaded <- liftIO $ Down.openURI $ toString url
    case downloaded of
      Left error -> throwError $ fromString error
      Right v    -> return v

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


