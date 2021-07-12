module Database.Mongo.Options
  ( InsertOptions()
  , defaultInsertOptions
  , setInsertWriteConcern
  , setInsertJournaled
  , UpdateOptions()
  , defaultUpdateOptions
  , setUpdateWriteConcern
  , setUpdateJournaled
  , setUpdateUpsert
  ) where

import Data.Maybe (Maybe(..))
import Database.Mongo.WriteConcern (WriteConcern)
import Simple.JSON (class WriteForeign, write)

-- | Typed options for inserting documents into a collection
newtype InsertOptions = InsertOptions
  { writeConcern :: Maybe WriteConcern
  , journaled    :: Maybe Boolean
  }

defaultInsertOptions :: InsertOptions
defaultInsertOptions = InsertOptions
  { writeConcern : Nothing
  , journaled    : Just false
  }

setInsertWriteConcern :: WriteConcern -> InsertOptions -> InsertOptions
setInsertWriteConcern wc (InsertOptions io) = InsertOptions (io { writeConcern = Just wc})

setInsertJournaled :: Maybe Boolean -> InsertOptions -> InsertOptions
setInsertJournaled j (InsertOptions io) = InsertOptions (io { journaled = j })

instance encodeJsonInsertOptions :: WriteForeign InsertOptions where
  writeImpl (InsertOptions {writeConcern, journaled}) =
    write { w: writeConcern, j: journaled }

-- | Typed options for updating documents into a collection
newtype UpdateOptions = UpdateOptions
  { writeConcern :: Maybe WriteConcern
  , journaled    :: Maybe Boolean
  , upsert       :: Maybe Boolean
  }

defaultUpdateOptions :: UpdateOptions
defaultUpdateOptions = UpdateOptions
  { writeConcern : Nothing
  , journaled    : Just false
  , upsert       : Just false
  }
  
setUpdateWriteConcern :: WriteConcern -> UpdateOptions -> UpdateOptions
setUpdateWriteConcern wc (UpdateOptions uo) = UpdateOptions (uo { writeConcern = Just wc})

setUpdateJournaled :: Maybe Boolean -> UpdateOptions -> UpdateOptions
setUpdateJournaled j (UpdateOptions uo) = UpdateOptions (uo { journaled = j })

setUpdateUpsert :: Maybe Boolean -> UpdateOptions -> UpdateOptions
setUpdateUpsert u (UpdateOptions uo) = UpdateOptions (uo { upsert = u })

instance encodeJsonUpdateOptions :: WriteForeign UpdateOptions where
  writeImpl (UpdateOptions o) =
    write { w: o.writeConcern, j: o.journaled, upsert: o.upsert }
