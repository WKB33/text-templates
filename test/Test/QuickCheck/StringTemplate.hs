{-|
Module      : StringTemplate
Description : Generation of random string templates
Copyright   : (c) Harley Eades, 2026
              ) WKB3, 2026
Maintainer  : harley.eades@gmail.com

Includes a generator for QuickCheck to randomly generate string templates to be
used for property-based testing.
-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.QuickCheck.StringTemplate
    (genTemplate
    ,genFilledTemplate) where

import GHC.TypeLits                         (Natural)
import Test.QuickCheck                      (Gen, Arbitrary (arbitrary), generate, frequency, sized)
import Test.QuickCheck.Instances.Text       ()
import Test.QuickCheck.Instances.Natural    ()
import Data.Text                            qualified as DT

import Data.StringTemplate.TemplateInternal

genChunk :: Gen Template
genChunk = chunk <$> arbitrary

genHole :: Gen Hole
genHole = do 
    i <- arbitrary :: Gen Natural
    pure $ (i,Nothing)

escapeFilling :: DT.Text -> DT.Text
escapeFilling t = DT.foldl update "" t
    where
        update r c = if c `elem` ['$','{','}'] 
                     then r DT.:> '\\' DT.:> c 
                     else r DT.:> c

genFilling :: Gen DT.Text
genFilling = do 
    escapeFilling <$> arbitrary 

genFilled :: Gen Hole
genFilled = do
    i <- arbitrary :: Gen Natural
    f <- genFilling
    pure (i,Just f)

genSomeHole :: Gen Hole
genSomeHole = sized $ \n -> 
    frequency
        [ (1, genHole)
        , (n, genFilled)
        ]

type HoleProps = ([Natural],Natural,[Natural],Natural) 

updateHoleProps :: Hole 
                -> HoleProps
                -> HoleProps
updateHoleProps (i,Nothing) (hls,nhls,fhls,nfhls) = (i:hls,nhls+1,fhls,nfhls)
updateHoleProps (i,Just _)  (hls,nhls,fhls,nfhls) = (hls,nhls,i:fhls,1+nfhls)

genTemplateNat :: (Gen Hole) -> Natural -> Gen Template
genTemplateNat _       0 = genChunk
genTemplateNat holeGen n = do (Template t hprops) <- genTemplateNat holeGen $ n - 1
                              h <- holeGen
                              c <- arbitrary :: Gen DT.Text
                              let t' = ICompose c h t
                              let hprops' = updateHoleProps h hprops
                              pure $ Template t' hprops'

genFilledTemplate :: Gen FilledTemplate
genFilledTemplate = FilledTemplate <$> (arbitrary >>= genTemplateNat genFilled)

genTemplate :: Gen Template
genTemplate = arbitrary >>= genTemplateNat genSomeHole

instance Arbitrary Template where
    arbitrary :: Gen Template
    arbitrary = genTemplate

instance Arbitrary FilledTemplate where
    arbitrary :: Gen FilledTemplate
    arbitrary = genFilledTemplate
    