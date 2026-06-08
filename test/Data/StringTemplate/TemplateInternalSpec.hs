{-|
Module      : TemplateInternalSpec
Description : Testing spec for the string template API
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@gmail.com

Various properties of the internals of the string templates API.
-}
module  Data.StringTemplate.TemplateInternalSpec (spec) where

import Test.Hspec            (describe, Spec )
import Test.QuickCheck       (Property, Testable (property), verboseCheck)
import Test.Hspec.QuickCheck (prop)

import Data.StringTemplate.TemplateInternal
import Test.QuickCheck.StringTemplate ()

prop_associativeCompose :: Template -> Template -> Template -> Property
prop_associativeCompose t1 t2 t3 = property $ t1 +> (t2 +> t3) == (t1 +> t2) +> t3

prop_identityCompose :: Template -> Property
prop_identityCompose t = property $ (t +> (chunk "")) == t && ((chunk "") +> t) == t

-- Write a function to get all the hole labels from a template.
-- Use this to plug every hole, then use this + match to show the template is
-- preserved under plugging.

-- Prop: match t (plug t) == True

spec :: Spec 
spec = do
    describe "quick properties:" $ do
        describe "composition" $ do
            prop "associativity" $
                prop_associativeCompose
            prop "identity" $
                prop_identityCompose
