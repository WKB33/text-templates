{-# LANGUAGE QuasiQuotes #-}
module Data.StringTemplate.QQInternalSpec (spec) where

import Test.Hspec
import Data.Functor.Identity                 (Identity)
import Data.StringTemplate.TemplateInternal
import Data.StringTemplate.QQInternal

testJSONChunk1 :: (Template Identity (),Template Identity ())
testJSONChunk1 = ([template|this is a chunk|],chunk "this is a chunk")

testJSONChunk2 :: (Template Identity (),Template Identity ())
testJSONChunk2 = ([template| |],Template (IChunk " ") ([],0,[],0))

testJSONChunk3 :: (Template Identity (),Template Identity ())
testJSONChunk3 = ([template|😩|],Template (IChunk "😩") ([],0,[],0))

testHole1 :: (Template Identity (),Template Identity ())
testHole1 = ([template|$1{}$2{}$3{}|],Template (ICompose "" (1, EmptyHole) (ICompose "" (2, EmptyHole) (ICompose "" (3, EmptyHole) (IChunk "")))) ([1,2,3],3,[],0))

testHole2 :: (Template Identity (),Template Identity ())
testHole2 = ([template|$1{}$2{}-$3{}|],Template (ICompose "" (1, EmptyHole) (ICompose "" (2, EmptyHole) (ICompose "-" (3, EmptyHole) (IChunk "")))) ([1,2,3],3,[],0))

testHole3 :: (Template Identity (),Template Identity ())
testHole3 = ([template|this $1{} and $2{} is $1{}|],Template (ICompose "this " (1, EmptyHole) (ICompose " and " (2, EmptyHole) (ICompose " is " (1, EmptyHole) (IChunk "")))) ([1,2],2,[],0))

testHole4 :: (Template Identity (),Template Identity ())
testHole4 = ([template|Hi $1{}!|], Template (ICompose "Hi " (1, EmptyHole) (IChunk "!")) ([1],1,[],0))

testHole5 :: (Template Identity (),Template Identity ())
testHole5 = ([template|Hi ❤️, $1{} ‼|], Template (ICompose "Hi ❤️, " (1, EmptyHole) (IChunk " ‼")) ([1],1,[],0))

testJSONHole1 :: (Template Identity (),Template Identity ())
testJSONHole1 = ([template|
{
    "forename": "$1{}", 
    "surname": "$2{}"
}
|],Template (ICompose "\n{\n    \"forename\": \"" ((1, EmptyHole)) (ICompose "\", \n    \"surname\": \"" ((2, EmptyHole)) (IChunk "\"\n}\n"))) ([1,2],2,[],0))

testJSONHole2 :: (Template Identity (),Template Identity ())
testJSONHole2 = ([template|{"forename":"$1{}","surname":"$2{}"}|],Template (ICompose "{\"forename\":\"" ((1, EmptyHole)) (ICompose "\",\"surname\":\"" ((2, EmptyHole)) (IChunk "\"}"))) ([1,2],2,[],0))

spec :: Spec 
spec = do
    describe "general tests:" $ do
        it "chunk test 1" $ do
            (fst testJSONChunk1) `shouldBe` (snd testJSONChunk1)
        it "chunk test 2" $ do
            (fst testJSONChunk2) `shouldBe` (snd testJSONChunk2)
        it "chunk test 3" $ do
            (fst testJSONChunk3) `shouldBe` (snd testJSONChunk3)
        it "hole test 1" $ do
            (fst testHole1) `shouldBe` (snd testHole1)
        it "hole test 2" $ do
            (fst testHole2) `shouldBe` (snd testHole2)
        it "hole test 3" $ do
            (fst testHole3) `shouldBe` (snd testHole3)
        it "hole test 4" $ do
            (fst testHole4) `shouldBe` (snd testHole4)
        it "hole test 5" $ do
            (fst testHole5) `shouldBe` (snd testHole5)
    describe "JSON tests:" $ do                    
        it "hole test 1" $ do
            (fst testJSONHole1) `shouldBe` (snd testJSONHole1)
        it "hole test 2" $ do
            (fst testJSONHole2) `shouldBe` (snd testJSONHole2)        
