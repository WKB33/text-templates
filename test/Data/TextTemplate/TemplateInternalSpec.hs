{-|
Module      : TemplateInternalSpec
Description : Testing spec for the text template API
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@gmail.com

Various properties of the internals of the text templates API.
-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module  Data.TextTemplate.TemplateInternalSpec (spec) where

import Test.Hspec            (describe, Spec )
import Test.QuickCheck       (Property, Testable (property), verboseCheck, Arbitrary)
import Test.Hspec.QuickCheck (prop)
import Data.Text             qualified as DT

import Data.TextTemplate.TemplateInternal
import Test.QuickCheck.TextTemplate

prop_associativeCompose :: Template () -> Template () -> Template () -> Property
prop_associativeCompose t1 t2 t3 = property $ t1 +> (t2 +> t3) == (t1 +> t2) +> t3

prop_identityCompose :: Template () -> Property
prop_identityCompose t = property $ (t +> empty) == t && (empty +> t) == t

spec :: Spec 
spec = do
    describe "QuickCheck properties:" $ do
        describe "composition" $ do
            prop "associativity" $
                prop_associativeCompose
            prop "identity" $
                prop_identityCompose