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

-- * Utilities

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

-- | Takes an action, catch the relevant exceptions and generate an appropriate DBError
wrap :: IO a -> IO (Either DBError a)
wrap action = handleException handleFormatError
            $ handleException handleResultError
            $ handleException handleSQLError
            $ handleException handleRSSError
            $ Right <$> action

-- * Misc

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

-- | Check if an exception is a RSSError exception
isRSSError :: Exception e => e -> Bool
isRSSError = isJust . convert . toException
 where convert :: SomeException -> Maybe RSSError
       convert = fromException

-- | Takes an action, catch the relevant exception specified by the handler
handleException :: Exception e => (e -> DBError)        -- ^ The handler, its type specified the
                                                        --   caught exception
                               -> IO (Either DBError a) -- ^ The action to wrap
                               -> IO (Either DBError a)
handleException handler process = try process >>= \case
    Left e           -> return $ Left $ handler e
    Right (Left dbe) -> return $ Left dbe
    Right (Right db) -> return $ Right db

-- * Handlers

-- | Handler for FormatError exception
handleFormatError :: FormatError -> DBError
handleFormatError = BadException . toException

-- | Handler for ResultError exception
handleResultError :: ResultError -> DBError
handleResultError = BadException . toException

-- | Handler for SQLError exception
handleSQLError :: SQLError -> DBError
handleSQLError = SQLFailure

-- | Handler for RSSError
handleRSSError :: RSSError -> DBError
handleRSSError (VersionMismatch ver) = BadVersion (majorVersion ver) (minorVersion ver)
handleRSSError TooManyVersions       = CorruptDB 
handleRSSError NoVersion             = CorruptDB
handleRSSError BadFormatMetadata     = CorruptDB

