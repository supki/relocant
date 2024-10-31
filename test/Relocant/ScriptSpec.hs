{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Relocant.ScriptSpec (spec) where

import Prelude hiding (id)
import System.FilePath ((</>))
import System.IO.Temp
import Test.Hspec

import Relocant.App.Env qualified as Env
import Relocant.Script
  ( Script(..)
  , parseFilePath
  , readContent
  , readScripts
  , readScript
  , apply
  )

import SpecHelper.DB qualified as DB


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
      withSystemTempDirectory Env.name $ \tmpDir -> do
        let
          path =
            tmpDir </> "a"
        writeFile path "abc"
        readContent path `shouldReturn` "abc"

  describe "readScript" $ do
    it "combines parseFilePath and readContent" $
      withSystemTempDirectory Env.name $ \tmpDir -> do
        let
          path =
            tmpDir </> "0001-migration-name.sql"
        writeFile path "abc"
        readScript path `shouldReturn`
          Script
            { id = "0001"
            , name = "0001-migration-name"
            , content = "abc"
            }

  describe "readScripts" $
    it "lists all .sql files in a directory, ordered by ID" $
      withSystemTempDirectory Env.name $ \tmpDir -> do
        let
          file name =
            tmpDir </> name
        writeFile (file "01-abc.sql") "abc"
        writeFile (file "02-def.sql") "def"
        writeFile (file "00-oops.sql") "oops"
        readScripts tmpDir `shouldReturn`
          [ Script
            { id = "00"
            , name = "00-oops"
            , content = "oops"
            }
          , Script
            { id = "01"
            , name = "01-abc"
            , content = "abc"
            }
          , Script
            { id = "02"
            , name = "02-def"
            , content = "def"
            }
          ]

  around DB.withTemplateCloned $ do
    describe "apply" $
      it "runs a migration script" $ \conn -> do
        let
          script = Script
            { id = "00"
            , name = "00-oops"
            , content = "CREATE TABLE foo ()"
            }
        _durationS <- apply script conn
        pure ()
