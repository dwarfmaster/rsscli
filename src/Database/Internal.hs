{-|
Module      : Database.Internal
Description : Handling the newsboat database
Copyright   : (c) Luc Chabassier, 2019
License     : MIT
Maintainer  : rsscli@dwarfmaster.net
Stability   : experimental
Portability : POSIX

Provides high level interface for the database.

-}

module Database.Internal where

import           Prelude                         ()
import           Relude
import           Relude.String.Conversion        (toString)
import           Database.SQLite.Simple          hiding (fold)
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow
import           Database.SQLite.Simple.ToField
import           Database.Errors
import           Database.Structure
import           Control.Exception
import           System.IO.Error                 (isDoesNotExistError)
import qualified Text.Show

-- | A constraint alias for types that represent a SQL row
--   (ie have conversion in both directions)
type RowAble a = (ToRow a, FromRow a)

-- | Filter named params list
filterNP :: (Text -> Bool) -> [NamedParam] -> [NamedParam]
filterNP pred = filter $ pred . getName
 where getName :: NamedParam -> Text
       getName (nm := _) = nm

--   ___       _ _    ----------------------------------------------------------
--  |_ _|_ __ (_) |_  ----------------------------------------------------------
--   | || '_ \| | __| ----------------------------------------------------------
--   | || | | | | |_  ----------------------------------------------------------
--  |___|_| |_|_|\__| ----------------------------------------------------------
--                    ----------------------------------------------------------

instance FromRow VersionRow where
    fromRow = VersionR <$> field <*> field
instance ToRow VersionRow where
    toRow (VersionR major minor) = toRow (major,minor)

-- | The actual version of the database this software is compatible with
dbVersion :: VersionRow
dbVersion = VersionR { majorVersion = 2, minorVersion = 11 }

-- | Get the connection object of a database
conn :: Database -> Connection
conn (DB _ c) = c

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

-- | Opens a database. It will create a new database if it doesn't exists.
openDatabase :: FilePath -> IO Database
openDatabase path = do
    db <- DB path <$> open path
    initialize db
    return db

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

-- | Parse flags entry
flags :: RowParser [Char]
flags = mkLst <$> field
 where mkLst :: Maybe [Char] -> [Char]
       mkLst Nothing  = []
       mkLst (Just x) = x

instance FromRow ItemRow where
    fromRow = ItemR <$> field <*> field <*> field <*> field <*> field
                    <*> field <*> field <*> field <*> field <*> field
                    <*> field <*> field <*> flags <*> field <*> field
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
    [ ":id"     := id,     ":guid"    := guid,    ":title"   := title,   ":author"   := author
    , ":url"    := url,    ":feedurl" := feedurl, ":pubdate" := pubdate, ":content"  := content
    , ":unread" := unread, ":eurl"    := eurl,    ":etype"   := etype,   ":enqueued" := enqueued
    , ":flags"  := flags,  ":deleted" := deleted, ":base"    := base ]

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
                   $ filterNP (/= ":id") $ itemNamed item
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

