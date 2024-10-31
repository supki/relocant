{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Relocant.AppliedSpec (spec) where

import Prelude hiding (id)
import Test.Hspec

import Relocant.Applied qualified as Applied
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
        Applied.record applied0 DB.defaultTable conn
        Applied.record applied1 DB.defaultTable conn
        [a0, a1] <- Applied.getApplied DB.defaultTable conn
        a0.id `shouldBe` "00"
        a0.bytes `shouldBe` "CREATE TABLE foo ()"
        a1.id `shouldBe` "01"
        a1.bytes `shouldBe` "CREATE TABLE bar ()"

    describe "selectByID" $
      it "select an applied migration by ID" $ \conn -> do
        let
          script = Script
            { id = "00"
            , name = "00-foo"
            , content = "CREATE TABLE foo ()"
            }
        applied <- Script.markApplied script
        Applied.record applied DB.defaultTable conn
        Just a <- Applied.getAppliedByID "00" DB.defaultTable conn
        a.id `shouldBe` "00"
        a.bytes `shouldBe` "CREATE TABLE foo ()"
        Nothing <- Applied.getAppliedByID "01" DB.defaultTable conn
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
        Applied.record applied DB.defaultTable conn
        [a] <- Applied.getApplied DB.defaultTable conn
        a.id `shouldBe` "00"
        a.bytes `shouldBe` "CREATE TABLE foo ()"

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
        Applied.record applied0 DB.defaultTable conn
        Applied.record applied1 DB.defaultTable conn
        Applied.deleteAll DB.defaultTable conn
        Applied.getApplied DB.defaultTable conn `shouldReturn` []

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
        Applied.record applied0 DB.defaultTable conn
        Applied.record applied1 DB.defaultTable conn
        True <- Applied.deleteByID "00" DB.defaultTable conn
        [a] <- Applied.getApplied DB.defaultTable conn
        a.id `shouldBe` "01"
        a.bytes `shouldBe` "CREATE TABLE bar ()"
