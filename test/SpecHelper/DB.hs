{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module SpecHelper.DB
  ( createTemplate
  , withTemplateCloned
  ) where

import Control.Exception (bracket, bracket_, catch)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.String (fromString)
import Database.PostgreSQL.Simple qualified as DB
import Database.PostgreSQL.Simple.SqlQQ qualified as DB (sql)
import Database.PostgreSQL.Simple.Types qualified as DB (QualifiedIdentifier)
import System.Random (randomIO)
import Text.Printf (printf)

import Relocant.DB qualified as DB (init)


createTemplate :: IO ()
createTemplate = do
  created <- bracket (DB.connectPostgreSQL "") DB.close $ \conn -> do
    createBase "relocant_base" conn
    pure True
   `catch`
    \DB.SqlError {DB.sqlErrorMsg = "database \"relocant_base\" already exists"} ->
      pure False
  when created $
    withDB "relocant_base" $ \conn -> do
      DB.init conn "public.relocant_migration"
      setTemplate "relocant_base" conn
      DB.close conn

withTemplateCloned :: (DB.Connection -> IO a) -> IO a
withTemplateCloned f = do
  n <- randomIO @Int32
  withClonedDB "relocant_base" (fromString (printf "relocant_test_%d" n)) f

withClonedDB :: String -> String -> (DB.Connection -> IO a) -> IO a
withClonedDB base cloned f = do
  bracket_
    (withDB (dbname base) (cloneTemplate (ident base) (ident cloned)))
    (withDB (dbname base) (dropDatabase (ident cloned)))
    (withDB (dbname cloned) f)
 where
  dbname :: String -> ByteString
  dbname = fromString
  ident :: String -> DB.QualifiedIdentifier
  ident = fromString

withDB :: ByteString -> (DB.Connection -> IO a) -> IO a
withDB name =
  bracket (DB.connectPostgreSQL ("dbname=" <> name)) DB.close

createBase :: DB.QualifiedIdentifier -> DB.Connection -> IO ()
createBase base conn = do
  _ <- DB.execute conn [DB.sql|
    CREATE DATABASE ?
  |] (DB.Only base)
  pure ()

setTemplate :: String -> DB.Connection -> IO ()
setTemplate base conn = do
  _ <- DB.execute conn [DB.sql|
    UPDATE pg_database
       SET datistemplate = true
     WHERE datname = ?
  |] (DB.Only base)
  pure ()

cloneTemplate :: DB.QualifiedIdentifier -> DB.QualifiedIdentifier -> DB.Connection -> IO ()
cloneTemplate base cloned conn = do
  _ <- DB.execute conn [DB.sql|
    CREATE DATABASE ? TEMPLATE ?
  |] (cloned, base)
  pure ()

dropDatabase :: DB.QualifiedIdentifier -> DB.Connection -> IO ()
dropDatabase name conn = do
  _ <- DB.execute conn [DB.sql|
    DROP DATABASE ?
  |] (DB.Only name)
  pure ()
