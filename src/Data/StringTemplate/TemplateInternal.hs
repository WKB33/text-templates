{-|
Module      : Template
Description : Framework for creating string templates
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@wkb3.com

Framework for creating string templates. These are strings with holes that 
can be filled and plugged. No parsing of the actual string is done, but the 
string is broken up into `chunk`'s in between the `hole`'s.
-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE TypeOperators                #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE RankNTypes                   #-}
{-# LANGUAGE TypeApplications             #-}
{-# LANGUAGE BangPatterns                 #-}
{-# LANGUAGE TypeAbstractions             #-}
{-# LANGUAGE TupleSections                #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.StringTemplate.TemplateInternal where

import Data.Text (Text)
import Data.Text qualified as DT
import GHC.Natural (Natural)
import Text.Megaparsec                      (Parsec
                                            ,between
                                            ,skipCount
                                            ,many
                                            ,some
                                            ,satisfy
                                            ,choice
                                            ,atEnd
                                            ,parseTest
                                            ,parse
                                            ,errorBundlePretty
                                            ,MonadParsec (try, takeWhile1P)
                                            ,(<|>), ShowErrorComponent (..), lookAhead, customFailure)
import Data.Char                            qualified as DT
import Text.Megaparsec.Char                 (char
                                            ,digitChar, space, string)
import Text.Megaparsec.Char.Lexer (symbol)
import Data.Char (isAsciiLower, isAlphaNum, isAscii)
import Data.Maybe (isNothing)
import Prelude                 hiding (null)
import Data.List               (union
                               ,delete)
import Control.Applicative     (Alternative)
import Data.String (IsString (..))

type Hole f = (Natural, Maybe f)

pattern EmptyHole :: Natural -> Hole f
pattern EmptyHole i <- (id -> (i,Nothing)) where
    EmptyHole i = (i,Nothing)

pattern FilledHole :: Natural -> f -> Hole f
pattern FilledHole i f <- (id -> (i,Just f)) where
    FilledHole i f = (i,Just f)

{-# COMPLETE EmptyHole, FilledHole #-}

class HoleFiller a where
    toFilling :: a -> Text

instance HoleFiller Text where
    toFilling :: Text -> Text
    toFilling = id

-- | Internal templates are the underlying structure of `Template`.
data ITemplate f where
    IChunk   :: Text -> ITemplate f
    ICompose :: Text -> Hole f -> ITemplate f -> ITemplate f

instance Show f => Show (ITemplate f) where
    show :: ITemplate f -> String    
    show (IChunk t)                     = DT.unpack t    
    show (ICompose   prefix (i,h) rest) = DT.unpack prefix <> "$" <> show i <> "{"
                                          <> maybe "" show h
                                          <> "}" <> show rest

-- | A template with pluggable holes. We do not expose the underlying
-- constructor in favor of the combinators.
data Template f where
    Template :: ITemplate f                      -- ^ Internal template
             -> ([Natural],Natural,[Natural],Natural) -- ^ Unfilled hole indices, number of unfilled holes, filled hole indices, and number of filled holes
             -> Template f

-- * Combinators

-- | Pattern synonym for the empty template.
pattern Empty :: Template f
pattern Empty <- (null -> True) where
    Empty = empty

-- | Pattern synonym for template chunk's.
pattern Chunk :: Text -> Template f
pattern Chunk s <- (Template (IChunk s) ([],0,[],0))
    where
        Chunk = chunk

-- | Pattern synonym for the composition of templates.
pattern Compose :: Text -> (Natural,Maybe f) -> Template f -> Template f
pattern Compose c h t <- (decompose -> Just (c, h, t))
    where
        Compose = (compose)

{-# COMPLETE Chunk, Compose #-}

-- | Explicitly create a top-level composition template.
compose :: Text             -- ^ Prefix chunk
        -> (Natural,Maybe f) -- ^ Hole index and potential filling
        -> Template f  -- ^ Template branch
        -> Template f
compose c (EmptyHole i)    t = chunk c +> hole i     +> t
compose c (FilledHole i f) t = chunk c +> filled i f +> t

-- | Decompose a template into the top-level compose.
decompose :: Template f -> Maybe (Text, (Natural, Maybe f), Template f)
decompose (Template (ICompose c h@(EmptyHole i)     t') (uh,nuh,fh,nfh)) = Just (c, h, (Template t' (i `delete` uh,nuh-1,fh,nfh)))
decompose (Template (ICompose c h@(FilledHole i _)  t') (uh,nuh,fh,nfh)) = Just (c, h, (Template t' (uh,nuh,i  `delete` fh,nfh-1)))
decompose _ = Nothing

-- | Test to see if a template is empty.
null :: Template f -> Bool
null (Template (IChunk "") ([],0,[],0)) = True
null _ = False

instance Show f => Show (Template f) where
    show :: Template f -> String
    show (Template t _) = show t

-- | Equality of `ITemplates`. The contents of filled holes are included in the
-- decision.
(>==>) :: Eq f 
       => ITemplate f
       -> ITemplate f
       -> Bool
(IChunk chk1)               >==> (IChunk chk2)               = chk1 == chk2
(ICompose   chk1 (_,c1) r1) >==> (ICompose   chk2 (_,c2) r2) = chk1 == chk2 && c1 == c2 && r1 >==> r2
_                           >==> _                           = False

instance Eq f => Eq (Template f) where
    (==) :: Template f -> Template f -> Bool
    (==) = (==>)

-- | Equality of templates. Two templates are considered equivalent if and only
-- if they differ by hole labels only. The contents of filled holes are included
-- in the decision.
(==>) :: Eq f
      => Template f
      -> Template f
      -> Bool
(Template t1 _) ==> (Template t2 _) = t1 >==> t2    

-- | An empty hole.
hole :: Natural -- ^ Hole index
     -> Template f
hole i = flip Template ([i],1,[],0) $ ICompose "" (EmptyHole i) (IChunk "")

-- | A holeilled with a concrete value. Throws a runtime exception if the
-- input string does not parse as a valid literal filling based on
-- `parseLitFilling`.
filled :: Natural    -- ^ Hole index
       -> f          -- ^ Hole filling
       -> Template f
filled i f 
    = flip Template ([],0,[i],1) $ (ICompose "" (FilledHole i f) (IChunk ""))

-- | A chunk is a substring to a larger string.
chunk :: Text -- ^ Substring.
      -> Template f
chunk = flip Template ([],0,[],0) .  IChunk

-- | The empty template corresponds to the empty string.
empty :: Template f
empty = chunk ""

-- | Composition of `ITemplates`.
(>+>) :: ITemplate f
      -> ITemplate f
      -> ITemplate f
(IChunk chk1)    >+> (IChunk chk2)    = IChunk $ chk1 <> chk2
(IChunk chk)     >+> (ICompose p h r) = ICompose (chk <> p) h r
(ICompose p h r) >+> t                = (ICompose p h $ r >+> t) 

-- | Composition of templates.
(+>) :: Template f
     -> Template f
     -> Template f
(Template t1 (ufhs1,m1,fhs1,n1)) +> (Template t2 (ufhs2,m2,fhs2,n2)) 
    = Template (t1 >+> t2) (ufhs1 `union` ufhs2,m1 + m2,fhs1 `union` fhs2,n1 + n2) 

instance Semigroup (Template f) where
    (<>) :: Template f -> Template f -> Template f
    (<>) = (+>)

instance Monoid (Template f) where
    mempty :: Template f
    mempty = empty

    mconcat :: [Template f] -> Template f
    mconcat = foldr (<>) empty

instance Functor Template where
    fmap :: (a -> b) -> Template a -> Template b
    fmap _ (Chunk t) = Chunk t
    fmap f (Compose p (EmptyHole i)    r) = Compose p (EmptyHole i) $ fmap f r
    fmap f (Compose p (FilledHole i h) r) = Compose p (FilledHole i . f $ h) $ fmap f r

instance IsString (Template f) where
    fromString :: String -> Template f
    fromString = Chunk . DT.pack

-- | Convert a templates AST into a `Text`. The `Show` instance for `Template`
-- is set to pretty print, but for debugging it is sometimes useful to see the
-- raw AST.
showAST :: Show f => Template f -> Text
showAST (Template (IChunk x) _)                          = "IChunk "   <> (DT.show x)
showAST (Template (ICompose p (EmptyHole h)     r) hls) = "ICompose " <> (DT.show p) <> " " <> (DT.show h) <> " (" <> (showAST (Template r hls)) <> ")"
showAST (Template (ICompose p (FilledHole h c)  r) hls) = "ICompose " <> (DT.show p) <> " " <> (DT.show h) <> " (" <> (DT.show c) <> ") (" <> (showAST (Template r hls)) <> ")"

-- | Get the list of unfilled-hole indices present in a template.
-- Time complexity: @O(0)@
unfilledHoles :: Template f -- ^ Template 
              -> [Natural]
unfilledHoles (Template _ (hls,_,_,_)) = hls

-- | Get the list of filled-hole indices present in a template.
-- Time complexity: @O(0)@
filledHoles :: Template f -- ^ Template 
            -> [Natural]
filledHoles (Template _ (_,_,fhls,_)) = fhls

-- | Get the number of unfilled holes in a template.
-- Time complexity: @O(0)@
numberOfUnfilledHoles :: Template f -- ^ Template 
                      -> Natural
numberOfUnfilledHoles (Template _ (_,m,_,_)) = m

-- | Get the number of filled holes in a template.
-- Time complexity: @O(0)@
numberOfFilledHoles :: Template f -- ^ Template 
                    -> Natural
numberOfFilledHoles (Template _ (_,_,_,n)) = n


-- | Decide if a template is filled or not. 
-- Time complexity: \(\mathcal{O}(1)\)
isFilled :: Template f -> Bool
isFilled t = numberOfUnfilledHoles t == 0

-- | Convert a template with no holes, a chunk, into a text.
-- Time complexity: @O(0)@
chunkToText :: Template f     
            -> Maybe Text
chunkToText (Template (IChunk c) ([],0,[],0)) = Just c
chunkToText _                                = Nothing

-- | Fill a hole with a text. Returns @Nothing@ if the hole index doesn't exist.
-- Filling a hole doesn't replace the hole, but simply puts the input text
-- inside the hole. 
fillHoleI :: ITemplate f
          -> Natural     -- ^ Hole index to plug
          -> f           -- ^ Hole filling
          -> Maybe (ITemplate f)
fillHoleI (ICompose p (h,_) t) i c | h == i = do
    t' <- fillHoleI t i c
    Just $ ICompose p (FilledHole h c) t'
fillHoleI (ICompose p hl t) i c = do
    t' <- fillHoleI t i c
    Just $ ICompose p hl t'
fillHoleI _ _ _ = Nothing

-- | Fill a hole with a text. Returns @Nothing@ if the hole index doesn't exist.
-- Filling a hole doesn't replace the hole, but simply puts the input text
-- inside the hole.
placeInHole :: Template f
            -> Natural     -- ^ Hole index to plug
            -> f           -- ^ Holeilling
            -> Maybe (Template f)
placeInHole (Template t@(ICompose _ _ _) st@(hls,m,fhls,n)) i c | i `elem` hls || i `elem` fhls = do 
    t' <- fillHoleI t i c
    if i `elem` hls
    then Just $ Template t' (i `delete` hls,m - 1,i:fhls,n + 1)
    else if i `elem` fhls
         then Just $ Template t' st
         else Nothing
placeInHole _ _ _ = Nothing

-- | Plug an unfilled hole in a template with some text. Returns @Nothing@ when
-- the hole index doesn't exist in the template or is filled, otherwise returns
-- a template with the hole plugged. Plugging a hole replaces the hole with the
-- value unlike `fillHole`.
plugHoleI :: HoleFiller f 
          => ITemplate f
          -> Natural   -- ^ Hole index to plug
          -> f         -- ^ Text to replace hole
          -> Maybe (ITemplate f)
plugHoleI (ICompose p (h,f) (IChunk s)) i c 
    | i == h && isNothing f = Just $ IChunk $ p <> toFilling c <> s
plugHoleI (ICompose p hl@(h,f) r@(ICompose p' h' s))  i c 
    | i == h && isNothing f = Just $ ICompose (p <> toFilling c <> p') h' s
    | otherwise = do r' <- plugHoleI r i c
                     Just $ ICompose p hl r'
plugHoleI _ _ _ = Nothing       

-- | Plug an unfilled hole in a template with some text. Returns @Nothing@ when
-- the hole index doesn't exist in the template or is filled, otherwise returns
-- a template with the hole plugged. Plugging a hole replaces the hole with the
-- value unlike `fillHole`.
plugHole :: HoleFiller f
         => Template f
         -> Natural  -- ^ Hole index to plug
         -> f        -- ^ Text to replace hole
         -> Maybe (Template f)
plugHole (Template t@(ICompose _ _ _) (hls,m,fhls,n)) i c | i `elem` hls = 
        do t' <- plugHoleI t i c
           pure $ Template t' (i `delete` hls,m - 1,fhls,n)
plugHole _ _ _ = Nothing

-- | Plugs every hole in a template with no filled holes using the given plug
-- function. If the plug function is defined for every hole in the input
-- template, then this function guarantees a template with no holes (a text).
plugAllI 
    :: HoleFiller f 
    => (Natural -> Maybe f)        -- ^ Plug function.
    -> ITemplate f                 -- ^ ITemplate to plug.
    -> Maybe (ITemplate f)
plugAllI f (ICompose chk (EmptyHole h) r) = do
    chk' <- f h
    IChunk chk'' <- plugAllI f r
    return . IChunk $ chk <> toFilling chk' <> chk''
plugAllI _ (ICompose _ (FilledHole _ _) _) = Nothing
plugAllI _ t@(IChunk _) = return t

-- | Plugs every hole in a template with no filled holes using the given plug
-- function. If the plug function is defined for every hole in the input
-- template, then this function guarantees a template with no holes (a text) is
-- returned.
plugAll :: HoleFiller f
        => Template f                           -- ^ Template to plug
        -> ([Natural] -> (Natural -> Maybe f))  -- ^ Plug function
        -> Maybe Text
plugAll (Template t (hls,_,[],0)) f = 
    case plugAllI (f hls) t of        
        Just (IChunk c) -> Just c
        _              -> Nothing
plugAll _ _ = Nothing

-- * Template Expressions

-- | Template Union of types. Use this to add string templates to various
-- locations within some structure data.
data TU a = StrTU (Template FillingExp) | LitTU a
    deriving Eq

class ToTemplateExp a where
    toTemplateExp :: a -> TemplateExp

instance ToTemplateExp TemplateExp where
    toTemplateExp :: TemplateExp -> TemplateExp
    toTemplateExp = id

instance ToTemplateExp a => ToTemplateExp (TU a) where
    toTemplateExp :: TU a -> TemplateExp
    toTemplateExp (StrTU t) = t
    toTemplateExp (LitTU l) = toTemplateExp l

-- | Hole fillings consist of meta-variables or literals which is anything that
-- can be converted into a `Data.Text.Text`.
data FillingExp 
    = VarFilling String  -- ^ Meta-variable
    | LitFilling Text    -- ^ Literal filling

instance Show FillingExp where
    show :: FillingExp -> String
    show (VarFilling x) = x
    show (LitFilling x) = show x

instance Eq FillingExp where
    -- | Equality is alpha-equivalence.
    (==) :: FillingExp -> FillingExp -> Bool
    (VarFilling _)  == (VarFilling _)  = True
    (LitFilling l1) == (LitFilling l2) = l1 == l2
    _               == _               = False

-- | A hole filling meta-variable.
varFexp :: String -> FillingExp
varFexp = VarFilling

-- | A literal hole filling.
literialFexp :: HoleFiller f => f -> FillingExp
literialFexp = LitFilling . toFilling

showASTFilling :: FillingExp -> Text
showASTFilling (LitFilling s) = "LitFilling "<>DT.show s
showASTFilling (VarFilling v) = "VarFilling "<>DT.show v

-- | A intermediate expression language for template strings where expressions
-- (`FillingExp`) fill their holes.
type TemplateExp = Template FillingExp

-- | Translates a list into a template list where each template in the input
-- list is separated by the input template.
sepTemplatesBy :: ToTemplateExp a
               => TemplateExp   -- ^ Separator
               -> [a] -- ^ List of templates
               -> TemplateExp
sepTemplatesBy _ []  = chunk ""
sepTemplatesBy _ [v] = toTemplateExp v
sepTemplatesBy sep (v:vs) = toTemplateExp v +> sep +> sepTemplatesBy sep vs 

-- | Add a prefix and suffix templates to the given value.
betweenTemplate :: ToTemplateExp a 
                => TemplateExp     -- ^ Prefix template
                -> TemplateExp     -- ^ Suffice template
                -> a              -- ^ Value to be converted into a template
                -> TemplateExp
betweenTemplate b a (toTemplateExp->t) = b +> t +> a

-- | Add brackets `[]` around the input template.
bracketTemplate :: TemplateExp -> TemplateExp
bracketTemplate = betweenTemplate (chunk "[") (chunk "]")

-- | Add braces `{}` around the input template.
braceTemplate :: TemplateExp -> TemplateExp
braceTemplate = betweenTemplate (chunk "{") (chunk "}")

-- | Parse a template union given a parser for templates and a parser for the
-- type being combined with templates.
parseTU :: Alternative m => m TemplateExp -> m a -> m (TU a)
parseTU tempParser p =  StrTU <$> tempParser
                    <|> LitTU <$> p

instance (Show a) => Show (TU a) where
    show :: TU a -> String
    show (StrTU t) = show t
    show (LitTU l) = show l

-- | Parse a template.
parseTemplate :: Text -> Either Text TemplateExp
parseTemplate s 
    = case parse templateParser "string-templates" s of
        Left bundle -> Left . DT.pack $ errorBundlePretty bundle
        Right t -> Right t

parseVarFilling :: Text -> Either Text String
parseVarFilling s 
    = case parse varFillingParser "string-templates" s of
        Left bundle -> Left . DT.pack $ errorBundlePretty bundle
        Right (VarFilling t) -> Right t
        _ -> error "StringTemplates.Parser: impossible branch reached in parseVarFilling."

parseLitFilling :: Text -> Either Text Text
parseLitFilling s 
    = case parse litFillingParser "string-templates" s of
        Left bundle -> Left . DT.pack $ errorBundlePretty bundle
        Right (LitFilling t) -> Right t
        _ -> error "StringTemplates.Parser: impossible branch reached in parseVarFilling."

-- | Convenient function for testing the parser in GHCi.
templateParserTest :: Text -> IO ()
templateParserTest = parseTest templateParser

-- | Parse errors
data ITParseError
    = ITPEVarFillingExpBeginLower
    deriving (Eq,Ord,Show)

instance ShowErrorComponent ITParseError where
    showErrorComponent :: ITParseError -> String
    showErrorComponent err = "string-templates: " <> showErrorComponent' err
        where
            showErrorComponent' ITPEVarFillingExpBeginLower = "filling variables must being with a lower-case letter"

-- | Type of tokens.
type Tok    = Text
-- | Type of the parsers that operate on a stream of `Tok`.
type Parser = Parsec ITParseError Tok

-- | Parse a hole index (`Natural`).
holeIndexParser :: Parser Natural
holeIndexParser = do
    ds <- some digitChar
    pure . read $ ds

charFillingParser :: Parser DT.Char
charFillingParser = choice [
        satisfy (\c -> c /= '"' && c /= '\\'),
        escapeCharFillingParser
    ]

escapeCharFillingParser :: Parser Char
escapeCharFillingParser = do
    skip (string "\\")
    satisfy (`elem` ['"','\\'])

stringFillingParser :: Parser Text
stringFillingParser = DT.pack <$> many charFillingParser

varFillingParser :: Parser FillingExp
varFillingParser = VarFilling <$> do
    -- Make sure we start with a lower-case ascii letter.
    c <- maybeParser . lookAhead $ takeWhile1P Nothing isAsciiLower
    if isNothing c
    then customFailure ITPEVarFillingExpBeginLower
    else DT.unpack <$> takeWhile1P Nothing (\c -> isAlphaNum c && isAscii c)

litFillingParser :: Parser FillingExp
litFillingParser = LitFilling <$> doubleQuotedParser stringFillingParser

fillingExpParser :: Parser FillingExp
fillingExpParser =  litFillingParser
                <|> varFillingParser

maybeParser :: MonadParsec e s f => f a -> f (Maybe a)
maybeParser p = try (Just <$> p) <|> pure Nothing

-- | Parse a hole's filling which must be escaped properly.
holeFillingParser :: Parser (Maybe FillingExp)
holeFillingParser = between (char '{') (char '}') $ maybeParser fillingExpParser

-- | Parse a `Hole`. That is, a pair of a hole index and a filling.
holeParser :: Parser (Hole FillingExp)
holeParser = do
    skip (string "$")
    i <- holeIndexParser
    f <- holeFillingParser
    pure $ (i, f)

-- | Parse a `Chunk`.
chunkParser :: Parser Text
chunkParser = DT.pack <$> many (templateCharParser False)

-- | Parse a template either as a `Chunk` or a `Compose`.
templateParser :: Parser TemplateExp
templateParser = do
    mc <- chunkParser
    isEnd <- atEnd
    if isEnd
    then pure . Chunk $ mc
    else do h <- holeParser
            t <- templateParser
            pure $ Compose mc h t

-- | Parse a template character. These are any unicode character where the
-- characters @['$','{','}','\\']@ are escaped when parsing a hole's filling,
-- otherwise just @'$'@ needs to be escaped.
templateCharParser :: Bool -> Parser DT.Char
templateCharParser filling = choice [
        satisfy (\c -> c /= '$' && c /= '\'' && (if filling then c /= '{' && c /= '}' else True) && c /= '\\'),
        escapedTemplateCharParser
    ]

-- | Parsed an escaped character; one of, @["\\$"","\\{"","\\}","\\\\"]@.
escapedTemplateCharParser :: Parser Char
escapedTemplateCharParser = do
    skipCount 1 (char '\\')
    satisfy (\c -> c == '$' || c == '{' || c == '}' || c == '\'')

-- * Helper parsers

-- | Parse a double-quoted output of the input parser.
doubleQuotedParser :: Parser a -> Parser a
doubleQuotedParser = between (string "\"") (tok "\"")

-- * Tokens

-- | Parse a token (unicode character)
-- Consumes whitespace *after* the parsed token.
tok :: Tok -> Parser Tok
tok = symbol space

-- | Parse and throw away the symbol parsed by the input token
skip :: Parser Tok -> Parser ()
skip = skipCount 1
