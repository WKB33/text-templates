{-# LANGUAGE QuasiQuotes #-}
module Data.StringTemplate.QQInternalSpec (spec) where

import Test.Hspec

import Data.StringTemplate
import Data.StringTemplate.TemplateInternal


testJSONChunk1 :: (Template,Template)
testJSONChunk1 = ([template|this is a chunk|],chunk "this is a chunk")

testJSONChunk2 :: (Template,Template)
testJSONChunk2 = ([template| |],Template $ Chunk " ")

testJSONChunk3 :: (Template,Template)
testJSONChunk3 = ([template|😩|],Template $ Chunk "😩")

testHole1 :: (Template,Template)
testHole1 = ([template|${1}${2}${3}|],Template $ Compose "" 1 (Compose "" 2 (Compose "" 3 (Chunk ""))))

testHole2 :: (Template,Template)
testHole2 = ([template|${1}${2}-${3}|],Template $ Compose "" 1 (Compose "" 2 (Compose "-" 3 (Chunk ""))))

testHole3 :: (Template,Template)
testHole3 = ([template|this ${1} and ${2} is ${1}|],Template $ Compose "this " 1 (Compose " and " 2 (Compose " is " 1 (Chunk ""))))

testHole4 :: (Template,Template)
testHole4 = ([template|Hi ${1}!|], Template $ Compose "Hi " 1 (Chunk "!"))

testHole5 :: (Template,Template)
testHole5 = ([template|Hi ❤️, ${1} ‼|], Template $ Compose "Hi ❤️, " 1 (Chunk " ‼"))

testJSONHole1 :: (Template,Template)
testJSONHole1 = ([template|
{
    "forename": "${1}", 
    "surname": "${2}"
}
|],Template $ Compose "\n{\n    \"forename\": \"" 1 (Compose "\", \n    \"surname\": \"" 2 (Chunk "\"\n}\n")))

testJSONHole2 :: (Template,Template)
testJSONHole2 = ([template|{"forename":"${1}","surname":"${2}"}|],Template $ Compose "{\"forename\":\"" 1 (Compose "\",\"surname\":\"" 2 (Chunk "\"}")))

-- Next tests:
-- 1. Plugging templates
-- 2. Matching against templates

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
