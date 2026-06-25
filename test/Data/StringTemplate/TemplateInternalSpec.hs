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
import Data.Functor.Identity (Identity)

prop_associativeCompose :: Template Identity () -> Template Identity () -> Template Identity () -> Property
prop_associativeCompose t1 t2 t3 = property $ t1 +> (t2 +> t3) == (t1 +> t2) +> t3

prop_identityCompose :: Template Identity () -> Property
prop_identityCompose t = property $ (t +> empty) == t && (empty +> t) == t

spec :: Spec 
spec = do
    describe "QuickCheck properties:" $ do
        describe "composition" $ do
            prop "associativity" $
                prop_associativeCompose
            prop "identity" $
                prop_identityCompose