{-|
Module      : JSONSpec
Description : Tests for JSON Templates
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@gmail.com

-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module  Data.StringTemplate.JSONInternalSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (property, Property)
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Natural ()
import Data.Text (Text)
import Data.Text qualified as DT
import Data.Char (isSpace, isControl, isPrint)
import Data.Maybe (isJust)
import Data.Functor.Identity (Identity)

import Data.StringTemplate.JSONInternal
import Data.StringTemplate.TemplateInternal (Template, (+>), chunk, hole, TU (LitTU, StrTU), filled)

spec :: Spec
spec = do    
    describe "literals" $ do
        test_case "empty"                                                test_empty
        test_case "boolean-true"                                         test_bool1
        test_case "boolean-false"                                        test_bool0
        test_case "parsing-null"                                         test_null        
        prop "parsing-numbers"                                           prop_numberParser
        test_case "leading-zeros"                                        test_number1    
        test_case "numbers-cannot-be-hex"                                test_number2
        test_case "parsing-escaped-quotes"                               test_quotedString        
        prop "parsing-strings"                                           prop_stringParser
        test_case "invalid-escape"                                       test_escape1
        test_case "invalid-string-newline"                               test_escape2
        test_case "invalid-string-invalid-escape"                        test_escape3
        test_case "unescaped-single-quote"                               test_escape4
        test_case "unescaped-tabs"                                       test_escape5
        test_case "bad-line-break"                                       test_escape6
        test_case "backslash-bad-line-break"                             test_escape7
    describe "arrays:" $ do
        test_case "singleton-array"                                      test_array1
        test_case "mixed-type-array-with-1-hole"                         test_array2
        test_case "empty-array"                                          test_array3
        test_case "trailing-comma"                                       test_array4
        test_case "no-commas"                                            test_array5
        test_case "heterogenous-array"                                   test_array6
        test_case "various-whitespace"                                   test_array7
        test_case "deeply-nested-array"                                  test_array8
        test_case "mismatched-brackets"                                  test_array9
        test_case "missing-closing-bracket"                              test_array10
        test_case "unicode-elements"                                     test_array11
        test_case "double-trailing-comma"                                test_array12
        test_case "no-values-only-comma"                                 test_array13
        test_case "comma-after-close"                                    test_array14
        test_case "extra-closing-bracket"                                test_array15
        test_case "colon-instead-of-comma"                               test_array16
        test_case "bad-value"                                            test_array17
        test_case "bad-number"                                           test_array18
        test_case "bad-number-expression"                                test_array19
        test_case "bad-number-expression-signs"                          test_array20
        test_case "mismatched-brackets-wrong-close"                      test_array21
        test_case "mismatched-brackets-wrong-open"                       test_array22
    describe "objects:" $ do
        test_case "empty-object"                                         test_object1
        test_case "no-colon-after-label"                                 test_object2
        test_case "missing-closing-brace"                                test_object3
        test_case "singleton-object"                                     test_object4
        test_case "empty-field-label"                                    test_object5
        test_case "duplicate-keys"                                       test_object6
        test_case "various-whitespace"                                   test_object7
        test_case "unicode-in-fields"                                    test_object8
        test_case "unicode-in-fields-values-surrogate-pairs"             test_object9
        test_case "unquoted-key"                                         test_object10
        test_case "trailing-comma"                                       test_object11
        test_case "extra-value-after-closing-brace"                      test_object12
        test_case "invalid-expression"                                   test_object13
        test_case "invalid-function-call"                                test_object14
        test_case "double-colon-after-label"                             test_object15
        test_case "comma-instead-of-colon-after-label"                   test_object16
        test_case "comma-instead-of-closing-brace"                       test_object17
        test_case "mismatched-brackets-wrong-close"                      test_object18
        test_case "mismatched-brackets-wrong-open"                       test_object19
    describe "files:" $ do
        json1 <- runIO $ readFile "test/example-data/nativejson-benchmark/jsonchecker/pass01.json"
        test_case "file-should-pass1"                                   (test_parseFile json1)
    describe "large-files:" $ do
        json1 <- runIO $ readFile "test/example-data/github-public-repos.json"
        test_case "large-file-github-public-repos-7k-lines-7k-fields"    (test_parseFile json1)
        json2 <- runIO $ readFile "test/example-data/nativejson-benchmark/canada.json"
        test_case "large-file-canada-coordinates-2M-size"                (test_parseFile json2)
        json3 <- runIO $ readFile "test/example-data/nativejson-benchmark/twitter.json"
        test_case "large-file-twitter-data-15K-lines-13K-fields"         (test_parseFile json3)
        json4 <- runIO $ readFile "test/example-data/nativejson-benchmark/citm_catalog.json"
        test_case "large-file-citm_catalog-50K-lines-26K-fields-2M-size" (test_parseFile json4)
    describe "jsonTemplates:" $ do
        let value1 = 42
        test_case "template-function"                                    (test_templateFun1 value1)

-- | The empty string is an error.
test_empty :: UnitTest (Maybe Value)
test_empty = UnitTest {
         test_output = testParser valueParser ""
        ,test_result = Nothing
    }

-- * Literals
-- ** Booleans
test_bool1 :: UnitTest Text
test_bool1 = UnitTest {
         test_output = DT.show ([jsonTemplate|true|] :: Template Identity ())
        ,test_result = "true"
    }

test_bool0 :: UnitTest Text
test_bool0 = UnitTest {
         test_output = DT.show ([jsonTemplate|false|] :: Template Identity ())
        ,test_result = "false"
    }

-- ** Null
test_null :: UnitTest Text
test_null = UnitTest {
         test_output = DT.show ([jsonTemplate|null|] :: Template Identity ())
        ,test_result = "null"
    }

-- ** Strings
isJSONStr :: Text -> Bool
isJSONStr DT.Empty                  = True
isJSONStr ('\\' DT.:< x DT.:< rest) = isEscapeChar x       && isJSONStr rest
isJSONStr (x DT.:< rest)            = not (isEscapeChar x || isSpace x || isControl x) && isPrint x && isJSONStr rest

testParser :: Parser a -> Text -> Maybe a
testParser p = eitherToMaybe . parse p

test_quotedString :: UnitTest (Maybe Value)
test_quotedString = UnitTest {
         test_output = testParser strVParser "\"\\\"New\\\"\""
        ,test_result = Just . StrV $ "\"New\""
    }

prop_stringParser :: DT.Text -> Property
prop_stringParser s = property $ if isJSONStr s then _parse s == ans s else True
    where
        _parse = testParser strVParser . ("\""<>) . (<>"\"")
        ans = Just . StrV

test_escape1 :: UnitTest (Maybe Value)
test_escape1 = UnitTest {
         test_output = testParser jsonParser "5\\x5"
        ,test_result = Nothing
    }

test_escape2 :: UnitTest (Maybe Value)
test_escape2 = UnitTest {
         test_output = testParser jsonParser "\nfoobar"
        ,test_result = Nothing
    }

test_escape3 :: UnitTest (Maybe Value)
test_escape3 = UnitTest {
         test_output = testParser jsonParser "\\\"foobar\042\\\""
        ,test_result = Nothing
    }

test_escape4 :: UnitTest (Maybe Value)
test_escape4 = UnitTest {
         test_output = testParser jsonParser "'foo'"
        ,test_result = Nothing
    }

test_escape5 :: UnitTest (Maybe Value)
test_escape5 = UnitTest {
     test_output = testParser jsonParser "\"\t unescaped\t tabs\t here\""
    ,test_result = Nothing
}

test_escape6 :: UnitTest (Maybe Value)
test_escape6 = UnitTest {
     test_output = testParser jsonParser "\"foo\nbar\""
    ,test_result = Nothing
}

test_escape7 :: UnitTest (Maybe Value)
test_escape7 = UnitTest {
     test_output = testParser jsonParser "\"foo\\\nbar\""
    ,test_result = Nothing
}

-- ** Numbers
prop_numberParser :: Double -> Property
prop_numberParser n = property $ _parse n == ans n
    where
        _parse = testParser numVParser . DT.show
        ans = Just . NumV

test_number1 :: UnitTest (Maybe Value)
test_number1 = UnitTest {
         test_output = testParser jsonParser "05"
        ,test_result = Nothing
    }

test_number2 :: UnitTest (Maybe Value)
test_number2 = UnitTest {
         test_output = testParser jsonParser "0x15"
        ,test_result = Nothing
    }

-- * Arrays
test_array1 :: UnitTest Text
test_array1 = UnitTest {
         test_output = (DT.show ([jsonTemplate|["1"]|] :: Template Identity ()))
        ,test_result = "[\"1\"]"
    }

test_array2 :: UnitTest Text
test_array2 = UnitTest {
         test_output = DT.show ([jsonTemplate|["1",'$1{}',3]|] :: Template Identity ())
        ,test_result = "[\"1\",$1{},3]"
    }

-- | The type of a unit test corresponds to a pair of an output value and an
-- expected result.
data UnitTest a = UnitTest {
     test_output :: a -- ^ Output of a computation
    ,test_result :: a -- ^ Expected result of the test
}

test_array3 :: UnitTest (Maybe Value)
test_array3 = UnitTest {
         test_output = testParser arrayVParser $ "[]"
        ,test_result = Just . ArrayV $ []
    } 

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe (Left _)  = Nothing

test_array4 :: UnitTest (Maybe Value)
test_array4 = UnitTest {
         test_output = testParser arrayVParser $ "[1,]"
        ,test_result = Nothing
    }

test_array5 :: UnitTest (Maybe Value)
test_array5 = UnitTest {
         test_output = testParser arrayVParser $ "[1 2 3 4 5]"
        ,test_result = Nothing
    }

test_array6 :: UnitTest (Maybe Value)
test_array6 = UnitTest {
         test_output = testParser arrayVParser $ "[42, \"foo\", '$1{}', null, {\"f\":\"v\"}, [1,2,3]]"
        ,test_result = Just . ArrayV $ [
             LitTU (NumV 42)
            ,LitTU (StrV "foo")
            ,StrTU (hole 1)
            ,LitTU NullV
            ,LitTU (ObjV [("f",LitTU (StrV "v"))])
            ,LitTU (ArrayV [LitTU (NumV 1),LitTU (NumV 2),LitTU (NumV 3)])
         ]
    }

test_array7 :: UnitTest (Maybe Value)
test_array7 = UnitTest {
         test_output = testParser arrayVParser $ "[1,   2,  3,\n\t 4   \n\n\t]"
        ,test_result = testParser arrayVParser $ "[1,2,3,4]"
    }

test_array8 :: UnitTest (Maybe Value)
test_array8 = UnitTest {
         test_output = testParser arrayVParser $ "[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
        ,test_result = Just . ArrayV $ [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])]
    }

test_array9 :: UnitTest (Maybe Value)
test_array9 = UnitTest {
         test_output = testParser arrayVParser $ "[}"
        ,test_result = Nothing
    }

test_array10 :: UnitTest (Maybe Value)
test_array10 = UnitTest {
         test_output = testParser arrayVParser $ "[1,2,3,4"
        ,test_result = Nothing
    }

test_array11 :: UnitTest (Maybe Value)
test_array11 = UnitTest {
         test_output = testParser arrayVParser $ "[\"\\u0000\", \"\\uD83D\\uDE00\"]"
        ,test_result = Just . ArrayV $ [LitTU (StrV "\NUL"),LitTU (StrV "😀")]
    }

test_array12 :: UnitTest (Maybe Value)
test_array12 = UnitTest {
         test_output = testParser arrayVParser $ "[\"42\",,]"
        ,test_result = Nothing
    }

test_array13 :: UnitTest (Maybe Value)
test_array13 = UnitTest {
         test_output = testParser arrayVParser $ "[,]"
        ,test_result = Nothing
    }

test_array14 :: UnitTest (Maybe Value)
test_array14 = UnitTest {
         -- The top-level JSON parser checks consumes the entire input.
         test_output = testParser jsonParser $ "[\"foo\"],"
        ,test_result = Nothing
    }

test_array15 :: UnitTest (Maybe Value)
test_array15 = UnitTest {
         -- The top-level JSON parser checks consumes the entire input.
         test_output = testParser jsonParser $ "[\"foo\"]]"
        ,test_result = Nothing
    }

test_array16 :: UnitTest (Maybe Value)
test_array16 = UnitTest {
         -- The top-level JSON parser checks consumes the entire input.
         test_output = testParser jsonParser $ "[4:5]"
        ,test_result = Nothing
    }

test_array17 :: UnitTest (Maybe Value)
test_array17 = UnitTest {
         -- The top-level JSON parser checks consumes the entire input.
         test_output = testParser jsonParser $ "[1,false,truth]"
        ,test_result = Nothing
    }

test_array18 :: UnitTest (Maybe Value)
test_array18 = UnitTest {
         -- The top-level JSON parser checks consumes the entire input.
         test_output = testParser jsonParser $ "[0e]"
        ,test_result = Nothing
    }

test_array19 :: UnitTest (Maybe Value)
test_array19 = UnitTest {
         -- The top-level JSON parser checks consumes the entire input.
         test_output = testParser jsonParser $ "[0e+]"
        ,test_result = Nothing
    }

test_array20 :: UnitTest (Maybe Value)
test_array20 = UnitTest {
         -- The top-level JSON parser checks consumes the entire input.
         test_output = testParser jsonParser $ "[0e+-1]"
        ,test_result = Nothing
    }

test_array21 :: UnitTest (Maybe Value)
test_array21 = UnitTest {
         -- The top-level JSON parser checks consumes the entire input.
         test_output = testParser jsonParser $ "[1}"
        ,test_result = Nothing
    }

test_array22 :: UnitTest (Maybe Value)
test_array22 = UnitTest {
         -- The top-level JSON parser checks consumes the entire input.
         test_output = testParser jsonParser $ "{1]"
        ,test_result = Nothing
    }

-- * Objects
test_object1 :: UnitTest (Maybe Value)
test_object1 = UnitTest {
         test_output = testParser objVParser $ "{}"
        ,test_result = Just . ObjV $ []
    }

test_object2 :: UnitTest (Maybe Value)
test_object2 = UnitTest {
         test_output = testParser objVParser $ "{\"field1\" 1}"
        ,test_result = Nothing
    }

test_object3 :: UnitTest (Maybe Value)
test_object3 = UnitTest {
         test_output = testParser objVParser $ "{\"field1\": 1"
        ,test_result = Nothing
    }

test_object4 :: UnitTest (Maybe Value)
test_object4 = UnitTest {
         test_output = testParser objVParser $ "{\"field1\": 1}"
        ,test_result = Just . ObjV $ [("field1",LitTU (NumV 1))]
    }

test_object5 :: UnitTest (Maybe Value)
test_object5 = UnitTest {
         test_output = testParser objVParser $ "{\"\": 1}"
        ,test_result = Just . ObjV $ [("",LitTU (NumV 1))]
    }

test_object6 :: UnitTest (Maybe Value)
test_object6 = UnitTest {
         test_output = testParser objVParser $ "{\"a\": 1, \"a\": 2}"
        ,test_result = Nothing
    }

test_object7 :: UnitTest (Maybe Value)
test_object7 = UnitTest {
         test_output = testParser objVParser $ "\n  \t  \n\n  {\n\t\n\"a\": 1,\t \"a\"  \n\t: \n\t2}"
        ,test_result = Nothing
    }

test_object8 :: UnitTest (Maybe Value)
test_object8 = UnitTest {
         test_output = testParser objVParser $ "{\"\\u006E\\u0061\\u006D\\u0065\": \"\\u004A\\u0053\\u004F\\u004E\"}"
        ,test_result = Just . ObjV $ [("name",LitTU (StrV "JSON"))]
    }

test_object9 :: UnitTest (Maybe Value)
test_object9 = UnitTest {
         test_output = testParser objVParser $ "{\"J\\u0053ON\": \"J\\u0053\\u004F\\u004E \\uD83D\\uDE00 is \\u0067reat\\u0021\"}"
        ,test_result = Just (ObjV [("JSON",LitTU (StrV "JSON \128512 is great!"))])
    }

test_object10 :: UnitTest (Maybe Value)
test_object10 = UnitTest {
         test_output = testParser objVParser $ "{field1: 1}"
        ,test_result = Nothing
    }

test_object11 :: UnitTest (Maybe Value)
test_object11 = UnitTest {
         test_output = testParser objVParser $ "{\"f\": 1,}"
        ,test_result = Nothing
    }

test_object12 :: UnitTest (Maybe Value)
test_object12 = UnitTest {
         test_output = testParser jsonParser $ "{\"f\": 1} 42"
        ,test_result = Nothing
    }

test_object13 :: UnitTest (Maybe Value)
test_object13 = UnitTest {
         test_output = testParser jsonParser $ "{\"f\": 2 * 2}"
        ,test_result = Nothing
    }

test_object14 :: UnitTest (Maybe Value)
test_object14 = UnitTest {
         test_output = testParser jsonParser $ "{\"f\": g()}"
        ,test_result = Nothing
    }

test_object15 :: UnitTest (Maybe Value)
test_object15 = UnitTest {
         test_output = testParser objVParser $ "{\"field1\":: 1}"
        ,test_result = Nothing
    }

test_object16 :: UnitTest (Maybe Value)
test_object16 = UnitTest {
         test_output = testParser objVParser $ "{\"field1\", 1}"
        ,test_result = Nothing
    }

test_object17 :: UnitTest (Maybe Value)
test_object17 = UnitTest {
         test_output = testParser jsonParser $ "{\"field1\": 1,"
        ,test_result = Nothing
    }

test_object18 :: UnitTest (Maybe Value)
test_object18 = UnitTest {
         test_output = testParser jsonParser $ "{\"field1\": 1]"
        ,test_result = Nothing
    }

test_object19 :: UnitTest (Maybe Value)
test_object19 = UnitTest {
         test_output = testParser jsonParser $ "[\"field1\": 1}"
        ,test_result = Nothing
    }

-- | Simply, did it parse?
test_parseFile :: String -> UnitTest Bool
test_parseFile (DT.pack->json) = UnitTest {
         test_output = isJust $ testParser jsonParser json                     
        ,test_result = True
    } 

test_case :: (Show a, Eq a) 
          => String 
          -> UnitTest a 
          -> SpecWith ()
test_case label t = it label $ (test_output t) `shouldBe` (test_result t)

-- * Template Functions
test_templateFun1 :: Int -> UnitTest (Template Maybe Int)
test_templateFun1 v = UnitTest {
        test_output = [jsonTemplate|{"field1": '$1{v}'}|]
       ,test_result = chunk "{\"field1\":" +> filled 1 v +> chunk "}"
    }
