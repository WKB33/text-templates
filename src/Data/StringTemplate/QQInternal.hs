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
import Data.Char                  qualified as DT
import Data.Text                  qualified as DT

import Data.StringTemplate.TemplateInternal

-- | Parse the filling of a hole including an empty filling. This is an @LR(1)@
-- parsing algorithm; and hence, @O(n)@ in the size of the unparsed stream. 
parseFilling :: DT.Text -- ^ Unparsed stream
             -> Either DT.Text (DT.Text,DT.Text)
parseFilling = _parseFilling ""
    where
        _parseFilling :: DT.Text -- ^ Parsed stream 
                     -> DT.Text -- ^ Unparsed stream
                     -> Either DT.Text (DT.Text,DT.Text)
        -- Either both streams are empty or we parsed the remainder of the unparsed
        -- stream without every finding the hole termination symbol, but both result in
        -- the same error.
        _parseFilling _                   DT.Empty          = Left $ "parse error: unclosed hole, expected '}'"
        -- Stop when the hole termination symbol is found
        _parseFilling parsed              ('}'  DT.:< next) = Right (parsed,next)
        -- Initial state
        _parseFilling DT.Empty            (c    DT.:< next) 
            | c `elem` ['$','{']                            = Left $ "parse error: found "<> (DT.singleton c) <> " expected \\" <> (DT.singleton c)
            | otherwise                                     = _parseFilling (DT.singleton c) next
        -- Skip the escape characters
        _parseFilling (parsed DT.:> '\\') (c    DT.:< next) 
            | c `elem` ['$','{','}']                        = _parseFilling (parsed DT.:> c) next        
        _parseFilling parsed              (c   DT.:< next)  = _parseFilling (parsed DT.:> c) next

compParsed :: Maybe Template -> Maybe Template -> Maybe Template
compParsed Nothing   Nothing   = Nothing
compParsed (Just t1) Nothing   = Just t1
compParsed Nothing   (Just t2) = Just t2
compParsed (Just t1) (Just t2) = Just $ t1 +> t2

parseHoleLabel :: DT.Text -> Either DT.Text (Natural,DT.Text)
parseHoleLabel = _parseHoleLabel ""

_parseHoleLabel :: DT.Text -> DT.Text -> Either DT.Text (Natural,DT.Text)
_parseHoleLabel DT.Empty DT.Empty                        = Left "unexpected end of input, expected hole label"
_parseHoleLabel _        DT.Empty                        = Left "unexpected end of input"
_parseHoleLabel n        (c DT.:< next) | DT.isDigit c   = _parseHoleLabel (n DT.:> c) next
                                       | not (DT.null n) = Right (read . DT.unpack $ n, c DT.:< next)
                                       | otherwise       = Left $ "expected hole label, but found " <> (DT.singleton c)

parseChar :: Char -> DT.Text -> Either DT.Text DT.Text
parseChar c (n DT.:< next) | c == n = Right next
parseChar c _ = Left $ "expected " <> (DT.singleton c) 

_parseTemplate :: (Maybe Template,Maybe Char) -> DT.Text -> Either DT.Text Template
-- Stopping conditions: either completely empty input or a completely parsed
-- input stream
_parseTemplate (parsed, Nothing) DT.Empty = Right $ maybe (chunk "") id parsed
-- Found a hole
_parseTemplate (parsed, p) ('$' DT.:< next) | p /= Just '\\' = do
    (label,next') <- parseHoleLabel next
    next'' <- parseChar '{' next'
    (filling,next''') <- parseFilling next''
    let t = Just $ if DT.null filling then hole label else filled label filling
    let parsed' = parsed `compParsed` (maybe Nothing (Just . chunk . DT.singleton) p) 
                         `compParsed` t
    _parseTemplate (parsed', Nothing) next'''
-- Found an escaped '$'
_parseTemplate (parsed, _) ('$' DT.:< next) = do
    let parsed' = parsed `compParsed` (Just $ chunk "$")
    _parseTemplate (parsed', Nothing) next
-- Initial state
_parseTemplate (parsed, Nothing) (c DT.:< next) = 
    _parseTemplate (parsed, Just c) next
-- Parse the previous symbol
_parseTemplate (parsed, Just p)  next = do
    let t = Just $ chunk $ DT.singleton p
    let parsed' = parsed `compParsed` t
    _parseTemplate (parsed', Nothing) next

-- | Parse a template from a string. 
parseTemplate :: DT.Text -> Either DT.Text Template
parseTemplate = _parseTemplate (Nothing,Nothing)

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

-- | Convert an `ITemplate` into a Template Haskell expression.
iTemplate2QExp :: ITemplate -> Q Exp
iTemplate2QExp (Chunk chk) = do
    let chunk = TH.mkName "chunk"
    appCombinator1 chunk $ mkTextLit chk  
iTemplate2QExp (Compose p (h,Nothing) r) = do
    -- Compose p h r = (chunk p) +> (hole h) +> r
    let pExp      = iTemplate2QExp (Chunk p)
    let hExp      = appCombinator1 (TH.mkName "hole") (mkNaturalLit h)
    let rExp      = iTemplate2QExp r
    let compose   = appInfixCombinator (TH.mkName "+>")
    (pExp `compose` hExp) `compose` rExp
iTemplate2QExp (Compose p (h,Just f) r) = do
    -- Compose p (h,f) r = (chunk p) +> (filled h f) +> r
    let pExp      = iTemplate2QExp (Chunk p)
    let hExp      = appCombinator2 (TH.mkName "filled") (mkNaturalLit h) (mkTextLit f)
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

-- | Convert a `Template` into a Template Haskell expression.
template2QExp :: Template -> Q Exp
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