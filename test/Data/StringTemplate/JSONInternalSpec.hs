{-|
Module      : JSONSpec
Description : Tests for JSON Templates
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@gmail.com

-}
{-# LANGUAGE QuasiQuotes #-}
module  Data.StringTemplate.JSONInternalSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (property, Property, verboseCheck)
import Test.QuickCheck.Instances.Text ()
import Data.Text (Text)
import Data.Text qualified as DT
import Data.Char (isSpace, isControl, isPrint)
import Text.Megaparsec qualified as MP

import Data.StringTemplate.JSONInternal
import Data.StringTemplate.TemplateInternal

testArray1 :: (Text,Text)
testArray1 = (DT.show [jsonTemplate|["1"]|],"[\"1\"]")

testArray2 :: (Text,Text)
testArray2 = (DT.show [jsonTemplate|["1",'$1{}',3]|],"[\"1\",$1{},3]")

testBool1 :: (Text,Text)
testBool1 = (DT.show [jsonTemplate|true|],"true")

testBool0 :: (Text,Text)
testBool0 = (DT.show [jsonTemplate|false|],"false")

testNull :: (Text,Text)
testNull = (DT.show [jsonTemplate|null|],"null")

isJSONStr :: Text -> Bool
isJSONStr DT.Empty                  = True
isJSONStr ('\\' DT.:< x DT.:< rest) = isEscapeChar x       && isJSONStr rest
isJSONStr (x DT.:< rest)            = not (isEscapeChar x || isSpace x || isControl x) && isPrint x && isJSONStr rest

testParser :: Parser a -> Text -> Either ParseError a
testParser p = MP.parse p ""

prop_numberParser :: Double -> Property
prop_numberParser n = property $ parse n == ans n
    where
        parse = testParser numVParser . DT.show
        ans = Right . NumV

prop_stringParser :: DT.Text -> Property
prop_stringParser s = property $ if isJSONStr s then parse s == ans s else True
    where
        parse = testParser strVParser . ("\""<>) . (<>"\"")
        ans = Right . StrV

spec :: Spec
spec = do
    describe "literals" $ do
        describe "booleans:" $ do
            it "boolean-true" $ do
                (fst testBool1) `shouldBe` (snd testBool1)
            it "boolean-false" $ do
                (fst testBool0) `shouldBe` (snd testBool0)
        describe "null:" $ do
            it "parsing-null" $ do
                (fst testNull) `shouldBe` (snd testNull)
        describe "numbers:" $ do
            prop "parsing-numbers" $ do
                prop_numberParser
        describe "strings:" $ do
            prop "parsing-strings" $
                prop_stringParser
    describe "arrays:" $ do
        it "singleton-array" $ do
            (fst testArray1) `shouldBe` (snd testArray1)
        it "mixed-type-array-with-1-hole" $ do
            (fst testArray2) `shouldBe` (snd testArray2)
