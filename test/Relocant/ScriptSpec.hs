{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Relocant.ScriptSpec (spec) where

import "crypton" Crypto.Hash (SHA1, hash)
import Data.ByteString (ByteString)
import Prelude hiding (id)
import System.FilePath ((</>))
import System.IO.Temp
import Test.Hspec

import Relocant.Migration qualified as Migration
import Relocant.Migration.Interval (makeInterval_)
import Relocant.Script
  ( Script(..)
  , Content(..)
  , parseFilePath
  , readContent
  , listDirectory
  , run
  , recordApplied
  )
import Relocant.Script qualified as Script (readFile)
import Relocant.DB qualified as DB

import SpecHelper.DB qualified as DB
import Cfg qualified


spec :: Spec
spec = do
  describe "parseFilePath" $ do
    it "empty ID/name is an ID/name" $
      parseFilePath ".sql" `shouldBe` ("", "")

    it "parses a bare migration ID" $
      parseFilePath "0001.sql" `shouldBe` ("0001", "0001")

    it "parses a migration ID separated from the rest of the name" $
      parseFilePath "0001-migration-name.sql" `shouldBe` ("0001", "0001-migration-name")

    it "migration ID can be arbitrary (alphanumeric value)" $ do
      parseFilePath "abc4-migration-name.sql" `shouldBe` ("abc4", "abc4-migration-name")
      parseFilePath "a%&#-migration-name.sql" `shouldBe` ("a", "a%&#-migration-name")

    it "the .sql extenstion is optional (but recommended (for your sanity))" $
      parseFilePath "abc4-migration-name" `shouldBe` ("abc4", "abc4-migration-name")

  describe "readContent" $ do
    it "reads the file and returns the content and its hash" $
      withSystemTempDirectory Cfg.name $ \tmpDir -> do
        let
          path =
            tmpDir </> "a"
        writeFile path "abc"
        readContent path `shouldReturn`
          Content {bytes = "abc", sha1 = hash @ByteString @SHA1 "abc"}

  describe "readFile" $ do
    it "combines parseFilePath and readContent" $
      withSystemTempDirectory Cfg.name $ \tmpDir -> do
        let
          path =
            tmpDir </> "0001-migration-name.sql"
        writeFile path "abc"
        Script.readFile path `shouldReturn`
          Script
            { id = "0001"
            , name = "0001-migration-name"
            , content = Content {bytes = "abc", sha1 = hash @ByteString @SHA1 "abc"}
            }

  describe "listDirectory" $
    it "lists all .sql files in a directory, ordered by ID" $
      withSystemTempDirectory Cfg.name $ \tmpDir -> do
        let
          file name =
            tmpDir </> name
        writeFile (file "01-abc.sql") "abc"
        writeFile (file "02-def.sql") "def"
        writeFile (file "00-oops.sql") "oops"
        listDirectory tmpDir `shouldReturn`
          [ Script
            { id = "00"
            , name = "00-oops"
            , content = Content {bytes = "oops", sha1 = hash @ByteString @SHA1 "oops"}
            }
          , Script
            { id = "01"
            , name = "01-abc"
            , content = Content {bytes = "abc", sha1 = hash @ByteString @SHA1 "abc"}
            }
          , Script
            { id = "02"
            , name = "02-def"
            , content = Content {bytes = "def", sha1 = hash @ByteString @SHA1 "def"}
            }
          ]

  describe "db-backed" $
    beforeAll DB.createTemplate $ do
      around DB.withTemplateCloned $ do
        describe "run" $
          it "runs a migration script" $ \conn -> do
            let
              script = Script
                { id = "00"
                , name = "00-oops"
                , content = Content
                  { bytes = "CREATE TABLE foo ()"
                  , sha1 = hash @ByteString @SHA1 "CREATE TABLE foo ()"
                  }
                }
            run script conn

        describe "record-applied" $
          it "runs a migration script" $ \conn -> do
            let
              script = Script
                { id = "00"
                , name = "00-oops"
                , content = Content
                  { bytes = "CREATE TABLE foo ()"
                  , sha1 = hash @ByteString @SHA1 "CREATE TABLE foo ()"
                  }
                }
            durationS <- makeInterval_ (pure ())
            recordApplied DB.defaultTable script durationS conn
            [m] <- Migration.loadAll DB.defaultTable conn
            m.id `shouldBe` "00"
            m.bytes `shouldBe` "CREATE TABLE foo ()"

