{-|
Module      : Database
Description : Handling the newsboat database
Copyright   : (c) Luc Chabassier, 2019
License     : MIT
Maintainer  : rsscli@dwarfmaster.net
Stability   : experimental
Portability : POSIX

Provide high-level interface to open and update an sqlite database storing
RSS entries as newsboat does.

The database follow the following schema :

 * The @metadata@ table store the version. It has two columns, only one row and no indexes

     * @db_schema_version_major@ of type @INTEGER NOT NULL@, which value must be 2
     * @db_schema_version_minor@ of type @INTEGER NOT NULL@, which value must be 11 (TODO really ?)

 * The @rss_feed@ store the feeds. It has the following columns :

     * @rssurl@ of type @VARCHAR(1024) PRIMARY KEY NOT NULL@, it stores the url of the
       rss feed.
     * @url@ of type @VARCHAR(1024) NOT NULL@, it stores the url of the website of the feed.
     * @title@ of type @VARCHAR(1024) NOT NULL@, it stores the title of the feed.
     * @lastmodified@ of type @INTEGER(11) NOT NULL DEFAULT 0@, it stores the last value of
       the last-modified header if the server supports it, see
       <https://developer.mozilla.org/fr/docs/Web/HTTP/Headers/Last-Modified> for details (TODO
       understand format in the database).
     * @is_rtl@ of type @INTEGER(1) NOT NULL DEFAULT 0@ tells us if the feed should be rendered
       right-to-left (will not support for now).
     * @etag@ of type @VARCHAR(128) NOT NULL DEFAULT ""@, it stores the last value of the
       etag header if the server supports it, see 
       <https://en.wikipedia.org/wiki/HTTP_ETag> for details.

     It has two indexes :

     * @idx_rssurl@ on @rss_url@
     * @idx_lastmodified@ on @lastmodified@

 * The @rss_item@ store all the rss items from all feeds. It has the following columns :

     * @id@ of type @INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL@
     * @guid@ of type @VARCHAR(64) NOT NULL@, it is an identifier trying to uniquely identify
       the item. If present in the RSS file it is used. Otherwise the url is used (thus two
       items with the same url will be juged equivalent)
     * @title@ of type @VARCHAR(1024) NOT NULL@, the title of the item (mandatory in RSS and Atom)
     * @author@ of type @VARCHAR(1024) NOT NULL@, the author of the item (mandatory in Atom)
     * @url@ of type @VARCHAR(1024) NOT NULL@, the link of the item (mandatory in
       both RSS and Atom).
     * @feedurl@ of type @VARCHAR(1024) NOT NULL@, the url of the feed.
     * @pubDate@ of type @INTEGER NOT NULL@, the date of publication of the item.
     * @content@ of type @VARCHAR(65535) NOT NULL@, the content of the item.
     * @unread@ of type @INTEGER(1) NOT NULL@, mark if the item has been read.
     * @enclosure_url@ of type @VARCHAR(1024)@, a specific url extracted from the feed,
       probably a download link in case of podcasts (TODO)
     * @enclosure_type@ of type @VARCHAR(1024)@, the mime type of the content of
       @enclosed_url@. It is not always present !
     * @enqueued@ of type @INTEGER(1) NOT NULL DEFAULT 0@, newsboat internal, to be left untouched
       when present and always initialized to 0.
     * @flags@ of type @VARCHAR(52)@, a list of flag, a flag being simply a letter, either miniscule
       or majuscule (thus at most 52 differents flags).
     * @deleted@ of type @INTEGER(1) NOT NULL DEFAULT 0@, probably an internal of newsboat, items with
       a value of 1 will be remove from the database on closure
     * @base@ of type @VARCHAR(128) NOT NULL DEFAULT ""@, seems to be the same as the url or empty.
       Thus we won't set it and ignore it when present.

     Some more information about RSS and Atom field here :
     <https://en.wikipedia.org/wiki/RSS#RSS_compared_with_Atom>.

     It has three indexes :

     * @idx_guid@ on @guid@
     * @idx_feedurl@ on @feedurl@
     * @idx_deleted@ on @deletedd@
-}

module Database where

import           Prelude                         ()
import           Relude
import           Relude.String.Conversion        (toString)
import           Database.SQLite.Simple          hiding (fold)
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow
import           Database.SQLite.Simple.ToField
import           Control.Exception
import           System.IO.Error                 (isDoesNotExistError)
import qualified Text.Show

-- | A constraint alias for types that represent a SQL row
--   (ie have conversion in both directions)
type RowAble a = (ToRow a, FromRow a)

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

--   ___       _ _    ----------------------------------------------------------
--  |_ _|_ __ (_) |_  ----------------------------------------------------------
--   | || '_ \| | __| ----------------------------------------------------------
--   | || | | | | |_  ----------------------------------------------------------
--  |___|_| |_|_|\__| ----------------------------------------------------------
--                    ----------------------------------------------------------

-- | Represent a row in the @metadata@ table 
data VersionRow = VersionR
                { majorVersion :: Integer -- ^ The @db_schema_version_major@ column
                , minorVersion :: Integer -- ^ The @db_schema_version_minor@ column 
                } deriving (Show, Eq, Ord)

instance FromRow VersionRow where
    fromRow = VersionR <$> field <*> field
instance ToRow VersionRow where
    toRow (VersionR major minor) = toRow (major,minor)

-- | The actual version of the database this software is compatible with
dbVersion :: VersionRow
dbVersion = VersionR { majorVersion = 2, minorVersion = 11 }

-- | Stores the connection information to a database
data Database = DB FilePath Connection
instance Show Database where
    show (DB path _) = "<db " <> path <> ">"

-- | Get the connection object of a database
conn :: Database -> Connection
conn (DB _ c) = c

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


-- | Checks if a table exists in a database
tableExists :: Database -- ^ The database
            -> Text     -- ^ The name of the table
            -> IO Bool
tableExists db name = do
    results <- query_ (conn db)
                      "SELECT name FROM sqlite_master WHERE type='table' AND name='metadata'"
                          :: IO [[ Text ]]
    return $ results == [ [ "metadata" ] ]

-- | Check for the existence of the @metadata@ table. If present check of the
-- version is correct and throw an 'RSSError' exception if it is incorrect. If the
-- table is not present, create it as well as the @rss_feed@ and @rss_item@ tables.
initialize :: Database -> IO ()
initialize db = do
    exists <- tableExists db "metadata"
    if exists then checkVersion db
              else createTables db

-- | Check if the version stored in the @metadata@ is correct. Otherwise throws
-- an 'RSSError' error
checkVersion :: Database -> IO ()
checkVersion db = do
    versions <- tryJust (Just . mkBadFormat)
                      $ query_ (conn db)
                        "SELECT * FROM metadata"
                            :: IO (Either RSSError [ VersionRow ])
    case versions of
      Left err          -> throwIO err
      Right []          -> throwIO NoVersion
      Right [ version ] -> if version == dbVersion then return ()
                                                   else throwIO $ VersionMismatch version
      _                 -> throwIO TooManyVersions
 where mkBadFormat :: FormatError -> RSSError
       mkBadFormat = const BadFormatMetadata

-- | Create a table in the database from its name and a list of pairs of name*type
createTable :: Database             -- ^ The database
            -> Text                 -- ^ The name of the table to create
            -> NonEmpty (Text,Text) -- ^ The list of columns, a column being a pair name*type
            -> IO ()
createTable db name ((clnm, cltp) :| largs) = execute_ (conn db) $ fromString query
 where query :: String
       query = "CREATE TABLE " <> toString name
            <> " (" <> toString clnm <> " " <> toString cltp
            <> foldMap (\(nm,tp) -> ", " <> toString nm <> " " <> toString tp) largs
            <> ")"

-- | Create an index of a specific column of a specific table of the database
createIndex :: Database -- ^ The database
            -> Text     -- ^ The name of index to create
            -> Text     -- ^ The name of the table to create an index on
            -> Text     -- ^ The name of the column of the table to create an index on
            -> IO ()
createIndex db name table col =
    execute_ (conn db)
           $ fromString $ "CREATE INDEX " <> toString name
                       <> " ON " <> toString table <> "(" <> toString col <> ")"

-- | Creates the tables with the right formats and the indexes
createTables :: Database -> IO ()
createTables db = do
    createTable db "metadata"
                [ ( "db_schema_version_major" , "INTEGER NOT NULL" )
                , ( "db_schema_version_minor" , "INTEGER NOT NULL" )
                ]
    execute (conn db)
            "INSERT INTO metadata (db_schema_version_major,db_schema_version_minor) VALUES (?,?)"
            dbVersion

    createTable db "rss_feed"
                [ ( "rssurl"       , "VARCHAR(1024) PRIMARY KEY NOT NULL" )
                , ( "url"          , "VARCHAR(1024) NOT NULL"             )
                , ( "title"        , "VARCHAR(1024) NOT NULL"             )
                , ( "lastmodified" , "INTEGER(11) NOT NULL DEFAULT 0"     )
                , ( "is_rtl"       , "INTEGER(1) NOT NULL DEFAULT 0"      )
                , ( "etag"         , "VARCHAR(128) NOT NULL DEFAULT \"\"" )
                ]
    createIndex db "idx_rssurl"       "rss_feed" "rssurl"
    createIndex db "idx_lastmodified" "rss_feed" "lastmodified"

    createTable db "rss_item"
                [ ( "id"             , "INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL" )
                , ( "guid"           , "VARCHAR(64) NOT NULL"                       )
                , ( "title"          , "VARCHAR(1024) NOT NULL"                     )
                , ( "author"         , "VARCHAR(1024) NOT NULL"                     )
                , ( "url"            , "VARCHAR(1024) NOT NULL"                     )
                , ( "feedurl"        , "VARCHAR(1024) NOT NULL"                     )
                , ( "pubDate"        , "INTEGER NOT NULL"                           )
                , ( "content"        , "VARCHAR(65535) NOT NULL"                    )
                , ( "unread"         , "INTEGER(1) NOT NULL"                        )
                , ( "enclosure_url"  , "VARCHAR(1024)"                              )
                , ( "enclosure_type" , "VARCHAR(1024)"                              )
                , ( "enqueued"       , "INTEGER(1) NOT NULL DEFAULT 0"              )
                , ( "flags"          , "VARCHAR(52)"                                )
                , ( "deleted"        , "INTEGER(1) NOT NULL DEFAULT 0"              )
                , ( "base"           , "VARCHAR(128) NOT NULL DEFAULT \"\""         )
                ]
    createIndex db "idx_guid"    "rss_item" "guid"
    createIndex db "idx_feedurl" "rss_item" "feedurl"
    createIndex db "idx_deleted" "rss_item" "deleted"

-- | Opens a database. It will create a new database if it doesn't exists. It does not do any error
-- handling.
openDatabaseInternal :: FilePath -> IO Database
openDatabaseInternal path = do
    db <- DB path <$> open path
    initialize db
    return db

-- | Opens a database, with proper error handling.
openDatabase :: FilePath -> IO (Either DBError Database)
openDatabase path = handleException handleFormatError
                  $ handleException handleResultError
                  $ handleException handleSQLError
                  $ handleException handleRSSError
                  $ Right <$> openDatabaseInternal path
 where handleException :: Exception e => (e -> DBError)
                                      -> IO (Either DBError Database)
                                      -> IO (Either DBError Database)
       handleException handler process = try process >>= \case
           Left e           -> return $ Left $ handler e
           Right (Left dbe) -> return $ Left dbe
           Right (Right db) -> return $ Right db

       handleFormatError :: FormatError -> DBError
       handleFormatError = BadException . toException
       handleResultError :: FormatError -> DBError
       handleResultError = BadException . toException
       handleSQLError :: SQLError -> DBError
       handleSQLError = SQLFailure

       handleRSSError :: RSSError -> DBError
       handleRSSError (VersionMismatch ver) = BadVersion (majorVersion ver) (minorVersion ver)
       handleRSSError TooManyVersions       = CorruptDB 
       handleRSSError NoVersion             = CorruptDB
       handleRSSError BadFormatMetadata     = CorruptDB

-- | Close a database after removing all items with delete = 1
closeDatabase :: Database -> IO ()
closeDatabase (DB _ c) = do
    execute_ c "DELETE FROM rss_item WHERE deleted = 1"
    close c


--   _____             _  ------------------------------------------------------
--  |  ___|__  ___  __| | ------------------------------------------------------
--  | |_ / _ \/ _ \/ _` | ------------------------------------------------------
--  |  _|  __/  __/ (_| | ------------------------------------------------------
--  |_|  \___|\___|\__,_| ------------------------------------------------------
--                        ------------------------------------------------------

-- | Represents a row in the @rss_feed@ table
data FeedRow = FeedR
             { feedRssurl   :: Text    -- ^ Correspond to the @rssurl@ column
             , feedUrl      :: Text    -- ^ Correspond to the @url@ column
             , feedTitle    :: Text    -- ^ Correspond to the @title@ column
             , lastModified :: Integer -- ^ Correspond to the @lastmodified@ column (TODO better type)
             , feedIsRtl    :: Bool    -- ^ Correspond to the @is_rtl@ column
             , feedEtag     :: Text    -- ^ Correspond to the @etag@ column
             } deriving (Show,Eq)

instance FromRow FeedRow where
    fromRow = FeedR <$> field <*> field <*> field <*> field <*> field <*> field
instance ToRow FeedRow where
    toRow (FeedR rssurl url title lastm rtl etag) = toRow (rssurl, url, title, lastm, rtl, etag)

-- | Look for a feed based on its rssurl in a database
findFeed :: Database           -- ^ The database to look on
         -> Text               -- ^ The rssurl to look for
         -> IO (Maybe FeedRow) -- ^ Nothing if the feed couldn't be found, true otherwise
findFeed db rssurl =
    query (conn db) "SELECT * FROM rss_feed WHERE rssurl = ? LIMIT 1" (Only rssurl) >>= \case
        []  -> return Nothing
        f:_ -> return $ Just f

-- | List all feeds in the database
listFeeds :: Database -> IO [FeedRow]
listFeeds db = query_ (conn db) "SELECT * FROM rss_feed"

-- | Udpdate a feed or if it doesn't exists create it
updateFeed :: Database -- ^ The database to update
           -> FeedRow  -- ^ The row to enter (its rssurl is used to find the row to update)
           -> IO Bool  -- ^ True if the feed was created
updateFeed db feed = findFeed db (feedRssurl feed) >>= \case
    Just _ -> do
        execute (conn db) "UPDATE rss_feed       \
                          \SET url = ?,          \
                          \    title = ?,        \
                          \    lastmodified = ?, \
                          \    is_rtl = ?,       \
                          \    etag = ?          \
                          \WHERE rssurl = ?"
                ( feedUrl   feed, feedTitle feed, lastModified feed
                , feedIsRtl feed, feedEtag  feed, feedRssurl   feed )
        return False
    Nothing -> do
        execute (conn db) "INSERT INTO rss_feed VALUES (?, ?, ?, ?, ?, ?)"
                ( feedRssurl   feed, feedUrl   feed, feedTitle feed
                , lastModified feed, feedIsRtl feed, feedEtag  feed )
        return True

-- | Delete a feed from the database
deleteFeed :: Database -- ^ The database to delete the feed from
           -> Text     -- ^ The rssurl of the feed to delete
           -> IO ()
deleteFeed db rssurl =
    execute (conn db) "DELETE FROM rss_feed WHERE rssurl = ?" $ Only rssurl




--   ___ _                  ----------------------------------------------------
--  |_ _| |_ ___ _ __ ___   ----------------------------------------------------
--   | || __/ _ \ '_ ` _ \  ----------------------------------------------------
--   | || ||  __/ | | | | | ----------------------------------------------------
--  |___|\__\___|_| |_| |_| ----------------------------------------------------
--                          ----------------------------------------------------

-- | Represents a row in the @rss_item@ table
data ItemRow = ItemR
             { itemId            :: Integer    -- ^ Correspond to the @id@ column
             , itemGuid          :: Text       -- ^ Correspond to the @guid@ column
             , itemTitle         :: Text       -- ^ Correspond to the @title@ column
             , itemAuthor        :: Text       -- ^ Correspond to the @author@ column
             , itemUrl           :: Text       -- ^ Correspond to the @url@ column
             , itemFeedurl       :: Text       -- ^ Correspond to the @feedurl@ column
             , itemPubdate       :: Integer    -- ^ Correspond to the @pubDate@ column (TODO better type)
             , itemContent       :: Text       -- ^ Correspond to the @content@ column
             , itemUnread        :: Bool       -- ^ Correspond to the @unread@ column
             , itemEnclosureUrl  :: Maybe Text -- ^ Correspond to the @enclosure_url@ column
             , itemEnclosureType :: Maybe Text -- ^ Correspond to the @enclosure_type@ column
             , itemEnqueued      :: Bool       -- ^ Correspond to the @enqueued@ column
             , itemFlags         :: [ Char ]   -- ^ Correspond to the @flags@ column
             , itemDeleted       :: Bool       -- ^ Correspond to the @deleted@ column
             , itemBase          :: Text       -- ^ Correspond to the @base@ column
             } deriving (Show,Eq)

instance FromRow ItemRow where
    fromRow = ItemR <$> field <*> field <*> field <*> field <*> field
                    <*> field <*> field <*> field <*> field <*> field
                    <*> field <*> field <*> field <*> field <*> field
instance ToRow ItemRow where
    toRow (ItemR id     guid title author   url   feedurl pubdate content
                 unread eurl etype enqueued flags deleted base) =
           toRow ( id,      guid,     title,   author,   url  )
        <> toRow ( feedurl, pubdate,  content, unread,   eurl )
        <> toRow ( etype,   enqueued, flags,   deleted,  base )

-- | Create a 'NamedParam' list with all the content of an ItemRow
itemNamed :: ItemRow -> [NamedParam]
itemNamed (ItemR id   guid  title    author url     feedurl pubdate content unread
                 eurl etype enqueued flags  deleted base) =
    [ "id"     := id,     "guid"    := guid,    "title"   := title,   "author"   := author
    , "url"    := url,    "feedurl" := feedurl, "pubdate" := pubdate, "content"  := content
    , "unread" := unread, "eurl"    := eurl,    "etype"   := etype,   "enqueued" := enqueued
    , "flags"  := flags,  "deleted" := deleted, "base"    := base ]

-- | Get an item from its id
getItemFromID :: Database -> Integer -> IO (Maybe ItemRow)
getItemFromID db id =
    query (conn db) "SELECT * FROM rss_item WHERE id = ?" (Only id) >>= \case
        []  -> return Nothing
        i:_ -> return $ Just i

-- | Get an item from its guid
getItemFromGUID :: Database -> Text -> IO (Maybe ItemRow)
getItemFromGUID db guid =
    query (conn db) "SELECT * FROM rss_item WHERE guid = ?" (Only guid) >>= \case
        []  -> return Nothing
        i:_ -> return $ Just i

-- | List all the items of a specific feed
listFeedItems :: Database
              -> Text         -- ^ The rssurl of the feed
              -> IO [Integer] -- ^ The ids of the items
listFeedItems db rssurl =
    query (conn db) "SELECT id FROM rss_item WHERE feedurl = ?" (Only rssurl) >>= (return . mconcat)

-- | Update (or create if it is inexistent) an item
--   If the item is created, the id of the item given will not be considered.
updateItem :: Database
           -> ItemRow
           -> IO Integer -- ^ 0 if the item was updated or the new id if it was created
updateItem db item = getItemFromID db (itemId item) >>= \case
    Nothing -> do
        executeNamed (conn db) "INSERT INTO rss_item \
                               \VALUES ( null        \
                               \       , :guid       \
                               \       , :title      \
                               \       , :author     \
                               \       , :url        \
                               \       , :feedurl    \
                               \       , :pubdate    \
                               \       , :content    \
                               \       , :unread     \
                               \       , :eurl       \
                               \       , :etype      \
                               \       , :enqueued   \
                               \       , :flags      \
                               \       , :deleted    \
                               \       , :base       \
                               \       )"
                   $ itemNamed item
        nids <- query_ (conn db) "SELECT last_insert_rowid()" :: IO [[Integer]]
        case nids of
          (nid:_):_ -> return nid
          []        -> error "last_insert_rowid returned nothing, should NOT happen"
    Just _  -> do
        executeNamed (conn db) "UPDATE rss_item              \
                               \SET guid = :guid,            \
                               \    title = :title,          \
                               \    author = :author,        \
                               \    url = :url,              \
                               \    feedurl = :feedurl,      \
                               \    pubDate = :pubdate,      \
                               \    content = :content,      \
                               \    unread = :unread,        \
                               \    enclosure_url = :eurl,   \
                               \    enclosure_type = :etype, \
                               \    enqueued = :enqueued,    \
                               \    flags = :flags,          \
                               \    deleted = :deleted,      \
                               \    base = :base             \
                               \WHERE id = :id"
                   $ itemNamed item
        return 0

-- | Delete an item from the table (will do nothing if it doesn't exists)
deleteItem :: Database
           -> Integer  -- ^ The id of the item to delete
           -> IO ()
deleteItem db id =
    execute (conn db) "DELETE FROM rss_item WHERE id = ?" $ Only id

