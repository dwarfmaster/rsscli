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

-}

module Database
    ( -- * Opening and closing
      openDatabase, closeDatabase, dbVersion
      -- * Feeds
    , findFeed, listFeeds, updateFeed, deleteFeed
      -- * Items
    , getItemFromID, getItemFromGUID, listFeedItems, updateItem, deleteItem
    ) where

import           Database.Internal  hiding (dbVersion)
import qualified Database.Internal  as DI
import           Database.Errors
import           Database.Structure
import           Control.Arrow

-- | The version of the database this software is compatible with
dbVersion :: (Integer, Integer)
dbVersion = majorVersion &&& minorVersion $ DI.dbVersion

