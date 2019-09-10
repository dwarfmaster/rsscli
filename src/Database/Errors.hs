{-|
Module      : Database.Error
Description : Helper functions to handle error from the database
Copyright   : (c) Luc Chabassier, 2019
License     : MIT
Maintainer  : rsscli@dwarfmaster.net
Stability   : experimental
Portability : POSIX
-}

module Database.Errors where

import           Prelude                ()
import           Relude
import           Database.SQLite.Simple hiding (fold)
import           Database.Structure
import           Control.Exception
import           System.IO.Error        (isDoesNotExistError)

-- | Check if an exception is a ResultError exception
isResultError :: Exception e => e -> Bool
isResultError = isJust . convert . toException
 where convert :: SomeException -> Maybe ResultError
       convert = fromException

-- | Check if an exception is a FormatError exception
isFormatError :: Exception e => e -> Bool
isFormatError = isJust . convert . toException
 where convert :: SomeException -> Maybe FormatError
       convert = fromException

-- | Represent all the possibles error that can happen when handling a database
data DBError = IOError IOException        -- ^ An IO error happened
             | Unknown Text               -- ^ An unspecifed error
             | BadVersion Integer Integer -- ^ The database is of the wrong version, including the
                                          --   major and minor values of the database version
             | CorruptDB                  -- ^ The database tables have the wrong format
             | SQLFailure SQLError        -- ^ Error coming from the SQL database
             | BadException SomeException -- ^ An unknown exception has been caught, it IS a bug, please
                                          --   report to <https://github.com/dwarfmaster/rsscli/issues>
             deriving (Show)

-- | Exception specific to the logic of this program
data RSSError = VersionMismatch VersionRow -- ^ The version of the database is not the one expected
              | TooManyVersions            -- ^ The @metadata@ table has more than one entry
              | NoVersion                  -- ^ The @metadata@ table has no entry
              | BadFormatMetadata          -- ^ The @metadata@ does not have the expected format
              deriving (Show)
instance Exception RSSError where

-- | Check if an exception is a RSSError exception
isRSSError :: Exception e => e -> Bool
isRSSError = isJust . convert . toException
 where convert :: SomeException -> Maybe RSSError
       convert = fromException

-- | Opens a database, with proper error handling.
-- openDatabase :: FilePath -> IO (Either DBError Database)
-- openDatabase path = handleException handleFormatError
--                   $ handleException handleResultError
--                   $ handleException handleSQLError
--                   $ handleException handleRSSError
--                   $ Right <$> openDatabaseInternal path
--  where handleException :: Exception e => (e -> DBError)
--                                       -> IO (Either DBError Database)
--                                       -> IO (Either DBError Database)
--        handleException handler process = try process >>= \case
--            Left e           -> return $ Left $ handler e
--            Right (Left dbe) -> return $ Left dbe
--            Right (Right db) -> return $ Right db
-- 
--        handleFormatError :: FormatError -> DBError
--        handleFormatError = BadException . toException
--        handleResultError :: FormatError -> DBError
--        handleResultError = BadException . toException
--        handleSQLError :: SQLError -> DBError
--        handleSQLError = SQLFailure
-- 
--        handleRSSError :: RSSError -> DBError
--        handleRSSError (VersionMismatch ver) = BadVersion (majorVersion ver) (minorVersion ver)
--        handleRSSError TooManyVersions       = CorruptDB 
--        handleRSSError NoVersion             = CorruptDB
--        handleRSSError BadFormatMetadata     = CorruptDB

