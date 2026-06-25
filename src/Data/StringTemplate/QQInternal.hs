{-|
Module      : QQInternal
Description : Quasi-Quoter for Templates
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@wkb3.com

Includes parsers for templates as well as a quasi-quoter 
for generating templates at compile time.
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data.StringTemplate.QQInternal where

import GHC.Natural                (Natural)
import Language.Haskell.TH        (Q
                                  ,Exp
                                  ,Name)
import Language.Haskell.TH.Quote  (QuasiQuoter (..))
import Language.Haskell.TH        qualified as TH
import Data.Text                  qualified as DT

import Data.StringTemplate.TemplateInternal

-- * Quasi-Quoter for Templates 

-- | The quasi-quoter.
template :: QuasiQuoter
template = QuasiQuoter {
    quoteExp  = stringTemplate2QExp
   ,quotePat  = undefined
   ,quoteDec  = undefined
   ,quoteType = undefined
}

-- | Parses a template string into a Template Haskell expression.
stringTemplate2QExp :: String -- ^ String to parse as a template
                    -> TH.Q TH.Exp
stringTemplate2QExp =  flip (.) (parseTemplate . DT.pack) $ \case {
         Right t  -> template2QExp t
        ;Left err -> fail $ DT.unpack err
    } 

-- | Convert a `Hole` into a Template Haskell expression.
hole2QExp :: Hole FillingExp -> Q Exp
hole2QExp (i,Nothing)             = appCombinator1 (TH.mkName "hole")   (mkNaturalLit i)
hole2QExp (i,Just (VarFilling v)) = appCombinator2 (TH.mkName "filled") (mkNaturalLit i) $ TH.varE . TH.mkName $ v
hole2QExp (i,Just (LitFilling f)) = appCombinator2 (TH.mkName "filled") (mkNaturalLit i) $ TH.stringE . DT.unpack $ f

-- | Convert an `ITemplate` into a Template Haskell expression.
iTemplate2QExp :: ITemplate FillingExp -> Q Exp
iTemplate2QExp (IChunk chk) = do
    let chunk = TH.mkName "chunk"
    appCombinator1 chunk $ mkTextLit chk  
iTemplate2QExp (ICompose p h r) = do
    -- ICompose p h r = (chunk p) +> (hole h) +> r
    let pExp      = iTemplate2QExp (IChunk p)
    let hExp      = hole2QExp h
    let rExp      = iTemplate2QExp r
    let compose   = appInfixCombinator (TH.mkName "+>")
    (pExp `compose` hExp) `compose` rExp

-- | Apply an infix combinator to a two arguments.
appInfixCombinator :: TH.Quote m 
                   => Name  -- ^ Name of the combinator
                   -> m Exp -- ^ First argument expression
                   -> m Exp -- ^ Second argument expression
                   -> m Exp 
appInfixCombinator constName e1 e2 = TH.infixE (Just e1) (TH.varE constName) (Just e2)

-- | Convert a type that can be converted into a template into a Template
-- Haskell expression. Use this to create new quasi-quoters for types that
-- convert to template.
template2QExp :: TemplateExp -> Q Exp
template2QExp (Template it _) = iTemplate2QExp it

-- * Helpful Template Haskell combinators.

-- | Apply a combinator to a single argument.
appCombinator1 :: TH.Quote m 
               => Name  -- ^ Name of the combinator
               -> m Exp -- ^ Argument expression
               -> m Exp 
appCombinator1 constName = TH.appE (TH.varE constName) 

-- | Apply a combinator to two arguments.
appCombinator2 :: TH.Quote m 
               => Name  -- ^ Name of the combinator
               -> m Exp -- ^ First argument expression
               -> m Exp -- ^ Second argument expression
               -> m Exp 
appCombinator2 constName a1 a2 = (TH.varE constName) `TH.appE`  a1 `TH.appE` a2 

-- | Apply a combinator to three arguments.
appCombinator3 :: TH.Quote m 
               => Name  -- ^ Name of the combinator
               -> m Exp -- ^ First argument expression
               -> m Exp -- ^ Second argument expression
               -> m Exp -- ^ Third argument expression
               -> m Exp 
appCombinator3 constName a1 a2 a3 = (TH.varE constName) `TH.appE`  a1 `TH.appE` a2 `TH.appE` a3

-- | Convert a `Text` into a Template Haskell literal.
mkTextLit :: TH.Quote m 
          => DT.Text -- ^ Text to convert
          -> m Exp
mkTextLit = TH.litE . TH.StringL . DT.unpack

-- | Convert a `Natural` to a Template Haskell literal.
mkNaturalLit :: TH.Quote m 
            => Natural -- ^ Natural to convert
            -> m Exp
mkNaturalLit = TH.litE . TH.IntegerL . toInteger