{-|
Module      : Template
Description : Framework for creating text templates
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@wkb3.com

Framework for creating text templates. These are text with holes that 
can be filled and plugged. No parsing of the actual text is done, but the 
text is broken up into `chunk`'s in between the `hole`'s.
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
module Data.TextTemplate.TemplateInternal where

import Data.Text (Text)
import Data.Text qualified as DT
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
import qualified Data.IntMap as M
import Data.IntMap (IntMap, (!?))

type Hole f      = (Int,HoleProps f)
type HoleProps f = ([Int],IntMap f)

pattern EmptyHole :: Int -> HoleProps f -> Hole f
pattern EmptyHole i hlsProps <- (decomposeEmptyHole -> Just (i,hlsProps))

pattern FilledHole :: Int -> f -> HoleProps f -> Hole f
pattern FilledHole i f hlsProps <- (decomposeFilledHole -> Just (i,Just f,hlsProps))

pattern UndefHole :: Int -> HoleProps f -> Hole f
pattern UndefHole i hlsProps <- (decomposeUndefHole -> Just (i,hlsProps))

decomposeEmptyHole :: (Int, HoleProps f) -> Maybe (Int, HoleProps f)
decomposeEmptyHole h@(i,hlsProps) | emptyHole i hlsProps = Just h
                                  | otherwise = Nothing

decomposeFilledHole :: Hole f -> Maybe (Int, Maybe f, ([Int], IntMap f))
decomposeFilledHole (i,hlsProps@(_,fhls)) | filledHole i hlsProps = Just (i,fhls !? i,hlsProps)
                                          | otherwise             = Nothing

decomposeUndefHole :: Hole f -> Maybe (Hole f)
decomposeUndefHole h | isNothing (decomposeEmptyHole h) && isNothing (decomposeFilledHole h) = Just h
                     | otherwise = Nothing

{-# COMPLETE EmptyHole, FilledHole, UndefHole #-}

emptyHole :: Int -> HoleProps f -> Bool
emptyHole i (hls,fhls) = i `elem` hls && not (i `elem` M.keys fhls)

filledHole :: Int -> HoleProps f -> Bool
filledHole i (hls,fhls) = not (i `elem` hls) && i `elem` M.keys fhls

emptyHoleProps :: HoleProps f
emptyHoleProps = ([], M.empty)

-- | Internal templates are the underlying structure of `Template`.
data ITemplate where
    IChunk   :: Text -> ITemplate 
    ICompose :: Text -> Int -> ITemplate -> ITemplate

-- | A template with pluggable holes. We do not expose the underlying
-- constructor in favor of the combinators.
data Template f where
    Template :: ITemplate         -- ^ Internal template
             -> HoleProps f       -- ^ Empty holes and hole-filling map
             -> Template f

instance Show f => Show (Template f) where
    show :: Template f -> String    
    show (Template (IChunk t) _) = DT.unpack t    
    show (Template (ICompose prefix i rest) (emptyHoles, filledHoles))
        = DT.unpack prefix <> "$" <> show i <> "{"
                           <> (if i `elem` emptyHoles then "" else (show $ filledHoles M.! i))
                           <> "}" <> show (Template rest (emptyHoles, filledHoles))

-- * Combinators

-- | Pattern synonym for the empty template.
pattern Empty :: Template f
pattern Empty <- (null -> True) where
    Empty = empty

isChunk :: Template f -> Maybe Text
isChunk (Template (IChunk s) ([],m)) | M.null m = Just s
isChunk _ = Nothing

-- | Pattern synonym for template chunk's.
pattern Chunk :: Text -> Template f
pattern Chunk s <- (isChunk -> Just s)
    where
        Chunk = chunk

-- | Pattern synonym for the composition of templates.
pattern Compose :: Text -> (Int,Maybe f) -> Template f -> Template f
pattern Compose c h t <- (decompose -> Just (c, h, t))
    where
        Compose = compose

{-# COMPLETE Chunk, Compose #-}

-- | Explicitly create a top-level composition template.
compose :: Text             -- ^ Prefix chunk
        -> (Int,Maybe f)           
        -> Template f       -- ^ Template branch
        -> Template f
compose c (i, Nothing) t = chunk c +> hole i     +> t
compose c (i, Just f)  t = chunk c +> filled i f +> t

-- | Decompose a template into the top-level compose.
decompose :: Template f -> Maybe (Text, (Int,Maybe f), Template f)
decompose (Template (ICompose c i t') hlsProps) =     
     case (i,hlsProps) of
        (EmptyHole _ (uh,fh))    -> Just (c, (i,Nothing), Template t' (i `delete` uh,fh))
        (FilledHole _ f (uh,fh)) -> Just (c, (i,Just f),  Template t' (uh,i `M.delete` fh))
        (UndefHole  _ _)         -> Nothing
decompose _ = Nothing

-- | Test to see if a template is empty.
null :: Template f -> Bool
null (Template (IChunk "") ([],m)) | M.null m = True
null _ = False

-- | Equality of `ITemplates`. Hole indices are not included in the decision.
(>==>) :: ITemplate
       -> ITemplate
       -> Bool
(IChunk chk1)          >==> (IChunk chk2)          = chk1 == chk2
(ICompose   chk1 _ r1) >==> (ICompose   chk2 _ r2) = chk1 == chk2 && r1 >==> r2
_                      >==> _                      = False

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
(Template t1 (_,fhMap1)) ==> (Template t2 (_,fhMap2)) = t1 >==> t2 && fhMap1 == fhMap2

-- | An empty hole.
hole :: Int -- ^ Hole index
     -> Template f
hole i = flip Template ([i],M.empty) $ ICompose "" i (IChunk "")

-- | A hole filled with a value. 
filled :: Int    -- ^ Hole index
       -> f      -- ^ Hole filling
       -> Template f
filled i f 
    = flip Template ([],M.singleton i f) $ (ICompose "" i (IChunk ""))

-- | A chunk is a substring to a larger string.
chunk :: Text -- ^ Substring.
      -> Template f
chunk = flip Template ([],M.empty) .  IChunk

-- | The empty template corresponds to the empty string.
empty :: Template f
empty = chunk ""

-- | Composition of `ITemplates`.
(>+>) :: ITemplate 
      -> ITemplate 
      -> ITemplate 
(IChunk chk1)    >+> (IChunk chk2)    = IChunk $ chk1 <> chk2
(IChunk chk)     >+> (ICompose p h r) = ICompose (chk <> p) h r
(ICompose p h r) >+> t                = ICompose p h $ r >+> t

-- | Composition of templates.
(+>) :: Template f
     -> Template f
     -> Template f
(Template t1 (ufhs1,fhs1)) +> (Template t2 (ufhs2,fhs2)) 
    = Template (t1 >+> t2) (ufhs1 `union` ufhs2,fhs1 `M.union` fhs2) 

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
    fmap f (Template t (hls,fhls)) = Template t $ (hls,M.map f fhls)

instance IsString (Template f) where
    fromString :: String -> Template f
    fromString = Chunk . DT.pack

-- | Convert a templates AST into a `Text`. The `Show` instance for `Template`
-- is set to pretty print, but for debugging it is sometimes useful to see the
-- raw AST.
showAST :: Show f => Template f -> Text
showAST (Template (IChunk x) _)                  = "IChunk "   <> (DT.show x)
showAST (Template (ICompose p i r) hls@(_,fhls)) = "ICompose " <> (DT.show p) <> " " <> (DT.show i) <> " (" <> (DT.show $ fhls M.!? i) <> ") (" <> (showAST (Template r hls)) <> ")"

-- | Get the list of unfilled-hole indices present in a template.
-- Time complexity: @O(0)@
unfilledHoles :: Template f -- ^ Template 
              -> [Int]
unfilledHoles (Template _ (hls,_)) = hls


-- | Get the list of filled-hole indices present in a template.
-- Time complexity: @O(n)@
filledHoles :: Template f -- ^ Template 
            -> [Int]
filledHoles (Template _ (_,fhls)) = M.keys fhls

-- | Get the number of unfilled holes in a template.
-- Time complexity: @O(n)@
numberOfUnfilledHoles :: Template f -- ^ Template 
                      -> Int
numberOfUnfilledHoles (Template _ (hls,_)) = length hls

-- | Get the number of filled holes in a template.
-- Time complexity: @O(n)@
numberOfFilledHoles :: Template f -- ^ Template 
                    -> Int
numberOfFilledHoles (Template _ (_,fhls)) = M.size fhls

-- | Decide if a template is filled or not. 
-- Time complexity: \(\mathcal{O}(n)\)
isFilled :: Template f -> Bool
isFilled t = numberOfUnfilledHoles t == 0

-- | Convert a template with no holes, a chunk, into a text.
-- Time complexity: @O(0)@
chunkToText :: Template f     
            -> Maybe Text
chunkToText (Template (IChunk c) ([],fhls)) | M.null fhls = Just c
chunkToText _                                             = Nothing

-- | Like `fillHole`, but doesn't update an already filled hole's value.
placeInHole :: Template f
            -> Int     -- ^ Hole index to plug
            -> f       -- ^ Holeilling
            -> Maybe (Template f)
placeInHole t@(Template it hlsProps) i c =
    case (i,hlsProps) of
        EmptyHole  _   (hls,fhls) -> Just $ Template it $ (i `delete` hls,M.insert i c fhls)
        FilledHole _ _ _          -> Just $ t
        UndefHole  _   _          -> Nothing

-- | Fill a hole with a text. If the hole is already filled, then the filling is
-- updated with the new value. Filling a hole doesn't replace the hole, but
-- simply puts the input text inside the hole. Returns @Nothing@ if the hole
-- doesn't exist.
fillHole :: Template f
             -> Int     -- ^ Hole index to plug
             -> f       -- ^ Holeilling
             -> Maybe (Template f)
fillHole (Template t hlsProps) i c = 
    case (i,hlsProps) of
        EmptyHole  _   (hls,fhls) -> Just $ Template t (i `delete` hls,M.insert i c fhls)
        FilledHole _ _ (hls,fhls) -> Just $ Template t (hls,M.insert i c fhls)
        UndefHole  _   _          -> Nothing

class HoleFiller a where
    toFilling :: a -> Text

instance HoleFiller Text where
    toFilling :: Text -> Text
    toFilling = id

-- | Plug an unfilled hole in a template with some text. Returns @Nothing@ when
-- the hole index doesn't exist in the template or is filled, otherwise returns
-- a template with the hole plugged. Plugging a hole replaces the hole with the
-- value unlike `fillHole`.
plugHoleI :: HoleFiller f 
          => ITemplate
          -> [Int]               -- ^ List of unfilled holes
          -> Int                 -- ^ Hole index to plug
          -> f                   -- ^ Text to replace hole
          -> Maybe ITemplate
plugHoleI (ICompose p h (IChunk s)) hls i c 
    | i == h && h `elem` hls = Just $ IChunk $ p <> toFilling c <> s
plugHoleI (ICompose p h r@(ICompose p' h' s)) hls i c 
    | i == h && h `elem` hls = Just $ ICompose (p <> toFilling c <> p') h' s
    | otherwise = do r' <- plugHoleI r hls i c
                     Just $ ICompose p h r'
plugHoleI _ _ _ _ = Nothing       

-- | Plug an unfilled hole in a template with some text. Returns @Nothing@ when
-- the hole index doesn't exist in the template or is filled, otherwise returns
-- a template with the hole plugged. Plugging a hole replaces the hole with the
-- value unlike `fillHole`.
plugHole :: HoleFiller f
         => Template f
         -> Int  -- ^ Hole index to plug
         -> f        -- ^ Text to replace hole
         -> Maybe (Template f)
plugHole (Template t@(ICompose _ _ _) (hls,fhls)) i c | i `elem` hls = 
        do t' <- plugHoleI t hls i c
           pure $ Template t' (i `delete` hls,fhls)
plugHole _ _ _ = Nothing

-- | Plugs every hole in a template with no filled holes using the given plug
-- function. If the plug function is defined for every hole in the input
-- template, then this function guarantees a template with no holes (a text).
plugAllI 
    :: HoleFiller f 
    => [Int]
    -> (Int -> Maybe f)      -- ^ Plug function.
    -> ITemplate             -- ^ ITemplate to plug.
    -> Maybe ITemplate
plugAllI hls f (ICompose chk i r) | i `elem` hls = do
    chk' <- f i
    IChunk chk'' <- plugAllI hls f r
    return . IChunk $ chk <> toFilling chk' <> chk''
plugAllI _ _ (ICompose _ _ _) = Nothing
plugAllI _ _ t@(IChunk _) = return t


-- | Plugs every hole in a template with no filled holes using the given plug
-- function. If the plug function is defined for every hole in the input
-- template, then this function guarantees a template with no holes (a text) is
-- returned.
plugAll :: HoleFiller f
        => Template f                   -- ^ Template to plug
        -> ([Int] -> (Int -> Maybe f))  -- ^ Plug function
        -> Maybe Text
plugAll (Template t (hls,fhls)) f | M.null fhls = 
    case plugAllI hls (f hls) t of        
        Just (IChunk c) -> Just c
        _              -> Nothing
plugAll _ _ = Nothing

-- * Template Expressions

-- | A intermediate expression language for text templates where expressions
-- (`FillingExp`) fill their holes.
type TemplateExp = Template FillingExp

-- | Template Union of types. Use this to add templates to various
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
    = case parse templateParser "text-templates" s of
        Left bundle -> Left . DT.pack $ errorBundlePretty bundle
        Right t -> Right t

parseVarFilling :: Text -> Either Text String
parseVarFilling s 
    = case parse varFillingParser "text-templates" s of
        Left bundle -> Left . DT.pack $ errorBundlePretty bundle
        Right (VarFilling t) -> Right t
        _ -> error "TextTemplates.Parser: impossible branch reached in parseVarFilling."

parseLitFilling :: Text -> Either Text Text
parseLitFilling s 
    = case parse litFillingParser "text-templates" s of
        Left bundle -> Left . DT.pack $ errorBundlePretty bundle
        Right (LitFilling t) -> Right t
        _ -> error "TextTemplates.Parser: impossible branch reached in parseVarFilling."

-- | Convenient function for testing the parser in GHCi.
templateParserTest :: Text -> IO ()
templateParserTest = parseTest templateParser

-- | Parse errors
data ITParseError
    = ITPEVarFillingExpBeginLower
    deriving (Eq,Ord,Show)

instance ShowErrorComponent ITParseError where
    showErrorComponent :: ITParseError -> String
    showErrorComponent err = "text-templates: " <> showErrorComponent' err
        where
            showErrorComponent' ITPEVarFillingExpBeginLower = "filling variables must being with a lower-case letter"

-- | Type of tokens.
type Tok    = Text
-- | Type of the parsers that operate on a stream of `Tok`.
type Parser = Parsec ITParseError Tok

-- | Parse a hole index (`Int`).
holeIndexParser :: Parser Int
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
holeParser :: Parser (Int, Maybe FillingExp)
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
