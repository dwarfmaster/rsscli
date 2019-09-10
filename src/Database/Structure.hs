{-|
Module      : Database.Structure
Description : Defines the structure of the database
Copyright   : (c) Luc Chabassier, 2019
License     : MIT
Maintainer  : rsscli@dwarfmaster.net
Stability   : experimental
Portability : POSIX

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

module Database.Structure where

import           Prelude ()
import           Relude
import           Database.SQLite.Simple
import qualified Text.Show

-- | Stores the connection information to a database
data Database = DB FilePath Connection
instance Show Database where
    show (DB path _) = "<db " <> path <> ">"

-- | Represent a row in the @metadable 
data VersionRow = VersionR
                { majorVersion :: Integer -- ^ The @db_schema_version_major@ column
                , minorVersion :: Integer -- ^ The @db_schema_version_minor@ column 
                } deriving (Show, Eq, Ord)

-- | Represents a row in the @rss_feed@ table
data FeedRow = FeedR
             { feedRssurl   :: Text    -- ^ Correspond to the @rssurl@ column
             , feedUrl      :: Text    -- ^ Correspond to the @url@ column
             , feedTitle    :: Text    -- ^ Correspond to the @title@ column
             , lastModified :: Integer -- ^ Correspond to the @lastmodified@ column (TODO better type)
             , feedIsRtl    :: Bool    -- ^ Correspond to the @is_rtl@ column
             , feedEtag     :: Text    -- ^ Correspond to the @etag@ column
             } deriving (Show,Eq)

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
             , itemFlags         :: [Char]     -- ^ Correspond to the @flags@ column
             , itemDeleted       :: Bool       -- ^ Correspond to the @deleted@ column
             , itemBase          :: Text       -- ^ Correspond to the @base@ column
             } deriving (Show,Eq)

