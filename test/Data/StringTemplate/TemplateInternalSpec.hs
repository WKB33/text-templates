{-|
Module      : TemplateInternalSpec
Description : Testing spec for the string template API
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@gmail.com

Various properties of the internals of the string templates API.
-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module  Data.StringTemplate.TemplateInternalSpec (spec) where

import Test.Hspec            (describe, Spec )
import Test.QuickCheck       (Property, Testable (property), verboseCheck, Arbitrary)
import Test.Hspec.QuickCheck (prop)
import Data.Text             qualified as DT

import Data.StringTemplate.TemplateInternal
import Test.QuickCheck.StringTemplate

prop_associativeCompose :: Template -> Template -> Template -> Property
prop_associativeCompose t1 t2 t3 = property $ t1 +> (t2 +> t3) == (t1 +> t2) +> t3

prop_identityCompose :: Template -> Property
prop_identityCompose t = property $ (t +> empty) == t && (empty +> t) == t

prop_matchReflexivity :: FilledTemplate -> Property
prop_matchReflexivity (FilledTemplate t) = property $ match t tx == Matched
    where
        tx = maybe DT.Empty id $ filledToText t

-- Properties on HoleProps

spec :: Spec 
spec = do
    describe "quick properties:" $ do
        describe "composition" $ do
            prop "associativity" $
                prop_associativeCompose
            prop "identity" $
                prop_identityCompose
        describe "match" $ do
            prop "template preservation" 
                prop_matchReflexivity
