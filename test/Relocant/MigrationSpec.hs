{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Relocant.MigrationSpec (spec) where

import Prelude hiding (id)
import Test.Hspec

import Relocant.Migration qualified as Migration
import Relocant.Script (Script(..))
import Relocant.Script qualified as Script (markApplied)
import Relocant.DB qualified as DB

import SpecHelper.DB qualified as DB


spec :: Spec
spec =
  around DB.withTemplateCloned $ do
    describe "selectAll" $
      it "select all applied migrations" $ \conn -> do
        let
          script0 = Script
            { id = "00"
            , name = "00-foo"
            , content = "CREATE TABLE foo ()"
            }
          script1 = Script
            { id = "01"
            , name = "00-bar"
            , content = "CREATE TABLE bar ()"
            }
        applied0 <- Script.markApplied script0
        applied1 <- Script.markApplied script1
        Migration.record applied0 DB.defaultTable conn
        Migration.record applied1 DB.defaultTable conn
        [m0, m1] <- Migration.selectAll DB.defaultTable conn
        m0.id `shouldBe` "00"
        m0.bytes `shouldBe` "CREATE TABLE foo ()"
        m1.id `shouldBe` "01"
        m1.bytes `shouldBe` "CREATE TABLE bar ()"

    describe "selectByID" $
      it "select an applied migration by ID" $ \conn -> do
        let
          script = Script
            { id = "00"
            , name = "00-foo"
            , content = "CREATE TABLE foo ()"
            }
        applied <- Script.markApplied script
        Migration.record applied DB.defaultTable conn
        Just m <- Migration.selectByID "00" DB.defaultTable conn
        m.id `shouldBe` "00"
        m.bytes `shouldBe` "CREATE TABLE foo ()"
        Nothing <- Migration.selectByID "01" DB.defaultTable conn
        pure ()

    describe "record" $
      it "stores the script in the DB" $ \conn -> do
        let
          script = Script
            { id = "00"
            , name = "00-oops"
            , content = "CREATE TABLE foo ()"
            }
        applied <- Script.markApplied script
        Migration.record applied DB.defaultTable conn
        [m] <- Migration.selectAll DB.defaultTable conn
        m.id `shouldBe` "00"
        m.bytes `shouldBe` "CREATE TABLE foo ()"

    describe "deleteAll" $
      it "deletes all applied migrations" $ \conn -> do
        let
          script0 = Script
            { id = "00"
            , name = "00-foo"
            , content = "CREATE TABLE foo ()"
            }
          script1 = Script
            { id = "01"
            , name = "00-bar"
            , content = "CREATE TABLE bar ()"
            }
        applied0 <- Script.markApplied script0
        applied1 <- Script.markApplied script1
        Migration.record applied0 DB.defaultTable conn
        Migration.record applied1 DB.defaultTable conn
        Migration.deleteAll DB.defaultTable conn
        Migration.selectAll DB.defaultTable conn `shouldReturn` []

    describe "deleteByID" $
      it "deletes an applied migration by ID" $ \conn -> do
        let
          script0 = Script
            { id = "00"
            , name = "00-foo"
            , content = "CREATE TABLE foo ()"
            }
          script1 = Script
            { id = "01"
            , name = "00-bar"
            , content = "CREATE TABLE bar ()"
            }
        applied0 <- Script.markApplied script0
        applied1 <- Script.markApplied script1
        Migration.record applied0 DB.defaultTable conn
        Migration.record applied1 DB.defaultTable conn
        True <- Migration.deleteByID "00" DB.defaultTable conn
        [m] <- Migration.selectAll DB.defaultTable conn
        m.id `shouldBe` "01"
        m.bytes `shouldBe` "CREATE TABLE bar ()"
