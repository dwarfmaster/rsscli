{-|
Module      : Database
Description : Handling the newsboat database
Copyright   : (c) Luc Chabassier, 2019
License     : MIT
Maintainer  : rsscli@dwarfmaster.net
Stability   : experimental
Portability : POSIX

This module exports database handling function. To see more details about
how the database is handled behind the scenes, see the "Database.Internal" module.

All functions comes in two flavors, one that can throw exception, and one where exceptions
have been caught and converted to a 'DBError'.

-}

module Database
    ( -- * Definitions
      Database, ItemRow(..), FeedRow(..)
      -- * Opening and closing
    , openDatabase,   closeDatabase,   dbVersion
    , openDatabaseNT, closeDatabaseNT
      -- * Feeds
    , findFeed,   listFeeds,   updateFeed,   deleteFeed
    , findFeedNT, listFeedsNT, updateFeedNT, deleteFeedNT
      -- * Items
    , getItemFromID,   getItemFromGUID,   listFeedItems,   updateItem,   deleteItem
    , getItemFromIDNT, getItemFromGUIDNT, listFeedItemsNT, updateItemNT, deleteItemNT
      -- * Error handling
    , DBError (..)
    ) where

import           Prelude            ()
import           Relude
import           Database.Internal  hiding (dbVersion)
import qualified Database.Internal  as DI
import           Database.Errors
import           Database.Structure
import           Control.Arrow

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f .: g = curry $ f . uncurry g

-- | The version of the database this software is compatible with
dbVersion :: (Integer, Integer)
dbVersion = majorVersion &&& minorVersion $ DI.dbVersion

-- | See 'openDatabase' for how it works
openDatabaseNT :: FilePath -> IO (Either DBError Database)
openDatabaseNT = wrap . DI.openDatabase

-- | See 'closeDatabase' for how it works
closeDatabaseNT :: Database -> IO (Either DBError ())
closeDatabaseNT = wrap . DI.closeDatabase

-- | See 'findFeed' for how it works
findFeedNT :: Database -> Text -> IO (Either DBError (Maybe FeedRow))
findFeedNT = wrap .: DI.findFeed

-- | See 'listFeeds' for how it works
listFeedsNT :: Database -> IO (Either DBError [FeedRow])
listFeedsNT = wrap . DI.listFeeds

-- | See 'updateFeed' for how it works
updateFeedNT :: Database -> FeedRow -> IO (Either DBError Bool)
updateFeedNT = wrap .: DI.updateFeed

-- | See 'deleteFeed' for how it works
deleteFeedNT :: Database -> Text -> IO (Either DBError ())
deleteFeedNT = wrap .: DI.deleteFeed

-- | See 'getItemFromID' for how it works
getItemFromIDNT :: Database -> Integer -> IO (Either DBError (Maybe ItemRow))
getItemFromIDNT = wrap .: DI.getItemFromID

-- | See 'getItemFromGUID' for how it works
getItemFromGUIDNT :: Database -> Text -> IO (Either DBError (Maybe ItemRow))
getItemFromGUIDNT = wrap .: DI.getItemFromGUID

-- | See 'listFeedItems' for how it works
listFeedItemsNT :: Database -> Text -> IO (Either DBError [Integer])
listFeedItemsNT = wrap .: DI.listFeedItems

-- | See 'updateItem' for how it works
updateItemNT :: Database -> ItemRow -> IO (Either DBError Integer)
updateItemNT = wrap .: DI.updateItem

-- | See 'deleteItem' for how it works
deleteItemNT :: Database -> Integer -> IO (Either DBError ())
deleteItemNT = wrap .: DI.deleteItem

