{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : StringTemplate
Description : Generation of random string templates
Copyright   : (c) Harley Eades, 2026
              ) WKB3, 2026
Maintainer  : harley.eades@gmail.com

Includes a generator for QuickCheck to randomly generate string templates to be
used for property-based testing.
-}
module Test.QuickCheck.StringTemplate
    (genTemplate) where

import GHC.TypeLits                         (Natural)
import Test.QuickCheck                      (Gen, Arbitrary (arbitrary), generate, frequency)
import Test.QuickCheck.Instances.Text       ()
import Test.QuickCheck.Instances.Natural    ()
import Data.Text                            qualified as DT

import Data.StringTemplate.TemplateInternal

genChunk :: Gen Template
genChunk = chunk <$> arbitrary

genHole :: Gen Template
genHole = hole <$> arbitrary

genTemplateNat :: Natural -> Gen Template
genTemplateNat 0 = genChunk
genTemplateNat n = do (Template t (hls,nhls,fhls,nfhls)) <- genTemplateNat $ n - 1
                      h <- arbitrary :: Gen Natural
                      c <- arbitrary :: Gen DT.Text
                      let t' = Compose c (h,Nothing) t
                      pure $ Template t' (h : hls,nhls+1,fhls,nfhls)

genTemplate :: Gen Template
genTemplate = arbitrary >>= genTemplateNat

instance Arbitrary Template where
    arbitrary :: Gen Template
    arbitrary = genTemplate