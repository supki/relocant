{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module SpecHelper.DB
  ( createTemplate
  , dropTemplate
  , withTemplateCloned
  ) where

import Control.Exception (bracket, bracket_)
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
  withDB "relocant" (createDatabase "relocant_base")
  withDB "relocant_base" $ \conn -> do
    DB.init "public.relocant_migration" conn
    setTemplate "relocant_base" conn
    DB.close conn

dropTemplate :: IO ()
dropTemplate =
  withDB "relocant" $ \conn -> do
    unsetTemplate "relocant_base" conn
    dropDatabase "relocant_base" conn

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

createDatabase :: DB.QualifiedIdentifier -> DB.Connection -> IO ()
createDatabase base conn = do
  _ <- DB.execute conn [DB.sql|
    CREATE DATABASE ?
  |] (DB.Only base)
  pure ()

setTemplate :: DB.QualifiedIdentifier -> DB.Connection -> IO ()
setTemplate base conn = do
  _ <- DB.execute conn [DB.sql|
    ALTER DATABASE ? IS_TEMPLATE = true;
  |] (DB.Only base)
  pure ()

unsetTemplate :: DB.QualifiedIdentifier -> DB.Connection -> IO ()
unsetTemplate base conn = do
  _ <- DB.execute conn [DB.sql|
    ALTER DATABASE ? IS_TEMPLATE = false;
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
