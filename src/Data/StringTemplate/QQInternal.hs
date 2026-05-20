{-|
Module      : QQInternal
Description : Quasi-Quoter for Templates
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@wkb3.com

Include parsers for templates as well as a quasi-quoter 
for generating templates at compile time.
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data.StringTemplate.QQInternal where

import GHC.Natural                (Natural)
import GHC.Unicode                (isDigit)
import Language.Haskell.TH        (Q
                                  ,Exp
                                  ,Name)
import Language.Haskell.TH.Quote  (QuasiQuoter (..))
import Language.Haskell.TH        qualified as TH
import Data.Text                  qualified as DT

import Data.StringTemplate.TemplateInternal

-- * Parsing Templates

-- | Parse a template from a string. 
parseTemplate :: DT.Text -> Either DT.Text Template
-- Parse a hole
parseTemplate ('$'  DT.:< r) = do
    let (h',r') = parseHole r
    h <- h'
    if DT.null r'
    then return $ hole h
    else do t <- parseTemplate r'
            return $ (hole h) +> t
-- Parse escape
parseTemplate ('\\' DT.:< r) = do
    let (chk',r') = parseEscape r
    chk <- chk'
    if DT.null r'
    then return $ chunk chk
    else do t <- parseTemplate r'
            return $ (chunk chk) +> t
-- Parse chunk
parseTemplate (x DT.:< r) = do
    let (chk,r') = DT.span (`notElem` ['$','\\']) r
    if DT.null r'
    then return $ chunk (x DT.:< chk)
    else do 
            t <- parseTemplate r'
            return $ (chunk (x DT.:< chk)) +> t

parseTemplate DT.Empty = Left "unexpected end of input"

-- | Parse a hole into a `Natural` number label and the rest of the stream.
parseHole :: DT.Text -- ^ Stream to parse
          -> (Either DT.Text Natural,DT.Text)
parseHole ('{' DT.:< r) = 
    case DT.span isDigit r of
        (DT.Empty,_           ) -> (Left "invalid hole label expecting a number",r)
        (d       ,'}' DT.:< r') -> (Right . read . DT.unpack $ d,r')
        (d       ,x   DT.:< r') -> (Left $ "found hole label "<>d<>" but encountered an invalid symbol "<>(DT.singleton x),r')
        (d       ,DT.Empty    ) -> (Left $ "found hole label "<>d<>" but encountered an unexpected end of input",DT.Empty)
parseHole (x   DT.:< r) = (Left $ "invalid symbol "<>DT.singleton x<>" expecting {",r)
parseHole DT.Empty      = (Left "unexpected end of input",DT.Empty)

-- | Parse an escape symbol into the parsed symbol and the rest of the stream.
parseEscape :: DT.Text -- ^ Stream to parse
            -> (Either DT.Text DT.Text,DT.Text)
parseEscape ('$' DT.:< r) = (Right "$",r)
parseEscape (x   DT.:< r) = (Left $ "invalid escaped symbol "<>DT.singleton x<>" expecting $",r)
parseEscape DT.Empty      = (Left "unexpected end of input",DT.Empty)

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
stringTemplate2QExp = flip (.) (parseTemplate . DT.pack) $ \case {
         Right t  -> template2QExp t
        ;Left err -> fail $ DT.unpack err
    } 

-- | Converts an `ITemplate` constructor name into its corresponding combinator.
constName2Combinator :: ITemplate n -> Name
constName2Combinator = TH.mkName . \case {
         Chunk   _     -> "chunk"
        ;Compose _ _ _ -> "+>"
    }

-- | Convert an `ITemplate` into a Template Haskell expression.
iTemplate2QExp :: ITemplate n -> Q Exp
iTemplate2QExp t@(Chunk chk) = do
    let constName = constName2Combinator t 
    appCombinator1 constName $ mkTextLit chk  
iTemplate2QExp t@(Compose p h r) = do
    let constName = constName2Combinator t 
    let pExp      = mkTextLit p
    let hExp      = mkNaturalLit h
    let rExp      = iTemplate2QExp r
    appCombinator3 constName pExp hExp rExp

-- | Convert a `Template` into a Template Haskell expression.
template2QExp :: Template -> Q Exp
template2QExp (Template it) = iTemplate2QExp it

-- * Helpful Template Haskell combinators.

-- | Apply a combinator to a single argument.
appCombinator1 :: TH.Quote m 
               => Name  -- ^ Name of the combinator
               -> m Exp -- ^ Argument expression
               -> m Exp 
appCombinator1 constName = TH.appE (TH.varE constName) 

-- | Apply a combinator to a single argument.
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