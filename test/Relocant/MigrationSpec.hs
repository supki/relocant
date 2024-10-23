{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Relocant.MigrationSpec (spec) where

import Prelude hiding (id)
import Test.Hspec

import Relocant.Migration qualified as Migration
import Relocant.Migration.Interval (makeInterval_)
import Relocant.Script (Script(..), recordApplied)
import Relocant.DB qualified as DB

import SpecHelper.DB qualified as DB


spec :: Spec
spec =
  around DB.withTemplateCloned $ do
    describe "loadAll" $
      it "load all migrations" $ \conn -> do
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
        durationS <- makeInterval_ (pure ())
        recordApplied DB.defaultTable script0 durationS conn
        recordApplied DB.defaultTable script1 durationS conn
        [m0, m1] <- Migration.loadAll DB.defaultTable conn
        m0.id `shouldBe` "00"
        m0.bytes `shouldBe` "CREATE TABLE foo ()"
        m1.id `shouldBe` "01"
        m1.bytes `shouldBe` "CREATE TABLE bar ()"

    describe "loadByID" $
      it "load a migration by ID" $ \conn -> do
        let
          script = Script
            { id = "00"
            , name = "00-foo"
            , content = "CREATE TABLE foo ()"
            }
        durationS <- makeInterval_ (pure ())
        recordApplied DB.defaultTable script durationS conn
        Just m <- Migration.loadByID "00" DB.defaultTable conn
        m.id `shouldBe` "00"
        m.bytes `shouldBe` "CREATE TABLE foo ()"
        Nothing <- Migration.loadByID "01" DB.defaultTable conn
        pure ()

    describe "deleteAll" $
      it "deletes all migrations" $ \conn -> do
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
        durationS <- makeInterval_ (pure ())
        recordApplied DB.defaultTable script0 durationS conn
        recordApplied DB.defaultTable script1 durationS conn
        Migration.deleteAll DB.defaultTable conn
        Migration.loadAll DB.defaultTable conn `shouldReturn` []

    describe "deleteByID" $
      it "deletes all migrations" $ \conn -> do
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
        durationS <- makeInterval_ (pure ())
        recordApplied DB.defaultTable script0 durationS conn
        recordApplied DB.defaultTable script1 durationS conn
        True <- Migration.deleteByID "00" DB.defaultTable conn
        [m] <- Migration.loadAll DB.defaultTable conn
        m.id `shouldBe` "01"
        m.bytes `shouldBe` "CREATE TABLE bar ()"
