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

module RSS where

import           Prelude      ()
import           Relude
import           GHC.Generics
import qualified Text.Show

--   ____                 _     _               
--  |  _ \ _ __ _____   _(_) __| | ___ _ __ ___ 
--  | |_) | '__/ _ \ \ / / |/ _` |/ _ \ '__/ __|
--  |  __/| | | (_) \ V /| | (_| |  __/ |  \__ \
--  |_|   |_|  \___/ \_/ |_|\__,_|\___|_|  |___/
--                                              

-- | The provider class. Generic is necessary so that it can be converted
-- to/from Dhall.
class Generic a => Provider a where
    serialise   :: a -> Text       -- ^ Stores the providers as a single url
    deserialise :: Text -> Maybe a -- ^ Try to decode an url as a provider
    -- TODO can only be defined after RSSItem has been defined
    -- execute     :: a -> IO [RSSItem] -- ^ Execute the provider and get the RSS items

-- | Wrapper to store any provider
data AnyProvider where
    AnyP :: Provider a => a -> AnyProvider

instance Show AnyProvider where
    show (AnyP p) = toString $ serialise p


--   _____             _ 
--  |  ___|__  ___  __| |
--  | |_ / _ \/ _ \/ _` |
--  |  _|  __/  __/ (_| |
--  |_|  \___|\___|\__,_|
--                       

-- | Struture representing a feed
data Feed = Feed
          { fdName :: Text         -- ^ The name of the feed
          , fdTags :: [Text]       -- ^ The tags of the feed
          , fdUrl  :: AnyProvider  -- ^ The provider of the feed (used to identify the feed)
          } deriving (Show,Generic)


--   ___ _                 
--  |_ _| |_ ___ _ __ ___  
--   | || __/ _ \ '_ ` _ \ 
--   | || ||  __/ | | | | |
--  |___|\__\___|_| |_| |_|
--                         

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

