{-|
Module      : StringTemplate
Description : Generation of random string templates
Copyright   : (c) Harley Eades, 2026
              ) WKB3, 2026
Maintainer  : harley.eades@gmail.com

Includes a generator for QuickCheck to randomly generate string templates to be
used for property-based testing.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.QuickCheck.StringTemplate
    (genTemplate) where

import GHC.TypeLits                         (Natural)
import Test.QuickCheck                      (Gen, Arbitrary (arbitrary), generate, frequency, sized)
import Test.QuickCheck.Instances.Text       ()
import Test.QuickCheck.Instances.Natural    ()
import Data.Functor.Identity                (Identity)
import Data.Text                            qualified as DT

import Data.StringTemplate.TemplateInternal

genChunk :: Gen (Template Identity ())
genChunk = chunk <$> arbitrary

genHole :: Gen ((Natural,Identity ()))
genHole = do 
    i <- arbitrary :: Gen Natural
    pure $ (i,EmptyHole)

genSomeHole :: Gen ((Natural,Identity ()))
genSomeHole = sized $ \_ -> 
    frequency
        [ (1, genHole)
        ]

type HoleProps = ([Natural],Natural,[Natural],Natural) 

updateHoleProps :: (Natural,Identity ())
                -> HoleProps
                -> HoleProps
updateHoleProps (i,EmptyHole)    (hls,nhls,fhls,nfhls) = (i:hls,nhls+1,fhls,nfhls)
updateHoleProps (i,FilledHole _) (hls,nhls,fhls,nfhls) = (hls,nhls,i:fhls,1+nfhls)

genTemplateNat :: Gen ((Natural,Identity ())) -> Natural -> Gen (Template Identity ())
genTemplateNat _       0 = genChunk
genTemplateNat holeGen n = do (Template t hprops) <- genTemplateNat holeGen $ n - 1
                              h <- holeGen
                              c <- arbitrary :: Gen DT.Text
                              let t' = ICompose c h t
                              let hprops' = updateHoleProps h hprops
                              pure $ Template t' hprops'

genTemplate :: Gen (Template Identity ())
genTemplate = arbitrary >>= genTemplateNat genSomeHole

instance Arbitrary (Template Identity ()) where
    arbitrary :: Gen (Template Identity ())
    arbitrary = genTemplate
