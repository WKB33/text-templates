{-|
Module      : JSON Templates
Description : String templates for JSON
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@gmail.com

String templates for JSON. The main use of this library is to test JSON
encoders/decoders, but there could be more use cases. This API is designed with
respect to [RFC 8259: STD 90: The JavaScript Object Notation (JSON) Data
Interchange Format](https://www.rfc-editor.org/info/rfc8259/).

- We do not allow duplicate keys.
- We require single quotes to be escaped due to templates.
-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module  Data.StringTemplate.JSONInternal where

import Text.Megaparsec            (Parsec
                                  ,ParseErrorBundle
                                  ,between
                                  ,skipCount
                                  ,many
                                  ,satisfy
                                  ,choice
                                  ,(<|>)
                                  ,errorBundlePretty
                                  ,sepBy1
                                  ,sepBy
                                  ,MonadParsec (try, eof, lookAhead), customFailure, ShowErrorComponent, unexpected, (<?>), ParsecT, runParserT, count, takeWhileP)
import Data.Text                  (Text)
import Data.Text                  qualified as DT
import Data.Void                  (Void)
import Text.Megaparsec.Char       (string
                                  ,space, space1)
import Data.Char                  (isPrint, isHexDigit, chr, generalCategory, isDigit)
import Text.Megaparsec.Char.Lexer (float
                                  ,decimal
                                  ,symbol, signed)
import Language.Haskell.TH        qualified as TH
import Language.Haskell.TH.Quote  (QuasiQuoter(..))
import Language.Haskell.TH.Quote  qualified as TH
import Data.StringTemplate        ((+>)
                                  ,chunk
                                  ,Template
                                  ,TU(..)) 
import Data.StringTemplate.TemplateInternal qualified as StrT
import Data.StringTemplate.Text             qualified as StrT
import Data.StringTemplate.QQInternal       qualified as StrT
import Numeric (readHex)
import Text.Megaparsec.Error (ShowErrorComponent(..), ErrorItem (..))
import Data.List.NonEmpty (fromList)
import Control.Monad.State (State, evalState, MonadTrans (..), MonadState (..))
import Data.StringTemplate.TemplateInternal (TemplateExp, FillingExp, ToTemplateExp (..))

-- * JSON Syntax

-- | Type of fields of a JSON object.
type Field = (DT.Text,TU Value)

-- | JSON Value
data Value 
    = ObjV   [Field]    -- ^ Object
    | ArrayV [TU Value] -- ^ Array
    | StrV   DT.Text    -- ^ String    
    | NumV   Double     -- ^ Number
    | BoolV  Bool       -- ^ Boolean
    | NullV             -- ^ Null
    deriving (Show, Eq)

-- | Create a template for a JSON value.
value :: Value -> TemplateExp
value (ObjV   obj)   = object obj
value (ArrayV ary)   = array ary
value (StrV   s)     = chunk $ StrT.doubleQuote s
value (NumV   n)     = chunk . StrT.prettyDouble $ n
value (BoolV  True)  = chunk "true"
value (BoolV  False) = chunk "false"
value NullV          = chunk "null"

instance ToTemplateExp Value where
    toTemplateExp :: Value -> TemplateExp
    toTemplateExp = value

instance ToTemplateExp Field where
    toTemplateExp :: Field -> TemplateExp
    toTemplateExp = field

-- * Creating JSON templates

-- | Create a template from a JSON object.
object :: [Field] -- ^ List of fields of the object
       -> TemplateExp
object fields = StrT.betweenTemplate (chunk "{") (chunk "}") $ StrT.sepTemplatesBy (chunk ",") fields

--- | Create a template of a field of an object.
field :: Field -- ^ Field of the object
      -> TemplateExp
field (label,value) = fieldLabel label +> toTemplateExp value

-- | Create a template of a field label of a field of an object.
fieldLabel :: DT.Text -- ^ Label of the field
           -> TemplateExp
fieldLabel = chunk . (<> ":") . StrT.doubleQuote

-- | Create a template of an array value.
array :: [TU Value] -- ^ List of values of the array
      -> TemplateExp
array = StrT.bracketTemplate . StrT.sepTemplatesBy (chunk ",")

-- * Quasi-quoter for JSON templates

-- | The JSON Templates quasi-quoter.
jsonTemplate :: TH.QuasiQuoter
jsonTemplate = TH.QuasiQuoter {
     quoteExp = jsonTemplate2QExp
    ,quotePat = undefined
    ,quoteDec = undefined
    ,quoteType = undefined
}

-- | Parse and convert a string into a JSON template. First parses the input
-- string into the internal language of JSON values, and then converts the
-- parsed value into a template.
jsonTemplate2QExp :: String
                  -> TH.Q TH.Exp
jsonTemplate2QExp = flip (.) (parseJSONTemplate . DT.pack) $ \case {
         Right v  -> StrT.template2QExp . toTemplateExp $ v
        ;Left err -> fail $ DT.unpack err
    } 

-- * JSON Templates Parser

-- | Parse errors
data JTParseError 
    = JTPEUnicode DT.Text
    | JTPEInvalidEscapeChar
    | JTPEDuplicateField DT.Text
    | JTPELeadingZeros
    deriving (Eq,Ord,Show)

instance ShowErrorComponent JTParseError where
    showErrorComponent :: JTParseError -> String
    showErrorComponent (JTPEUnicode s)        = DT.unpack s
    showErrorComponent JTPEInvalidEscapeChar  = "invalid escape character"
    showErrorComponent (JTPEDuplicateField s) = "duplicate field: "<>(DT.unpack s)
    showErrorComponent JTPELeadingZeros       = "invalid number: leading zeros are not allowed"

-- | Type of tokens.
type Tok    = DT.Text
-- | Type of parse errors.
type ParseError = ParseErrorBundle Tok JTParseError
-- | Type of the parsers that operate on a stream of `Tok`. The state holds onto
-- which fields have been parsed when parsing an object.
type Parser a = ParsecT JTParseError Tok (State [DT.Text]) a

-- | Parse a string using the input parser.
parse :: Parser a -> DT.Text -> Either ParseError a
parse p s = evalState (runParserT p "" s) []

-- | Test a parser on some input. Useful for testing parsers in GHCi.
parseTest :: Show a => Parser a -> DT.Text -> IO ()
parseTest p s = do
    either 
        (putStr . errorBundlePretty) 
        print 
    $ parse p s

parseTestFile :: Show a => Parser a -> FilePath -> IO ()
parseTestFile p file = do
    f <- readFile file
    parseTest p (DT.pack f)

-- | The JSON parser.
parseJSONTemplate 
    :: DT.Text -- ^ Text to parse
    -> Either DT.Text Value
parseJSONTemplate (DT.stripStart->s) 
    = case parse valueParser s of
        Left bundle -> error $ errorBundlePretty bundle
        Right s -> Right s

jsonParser :: Parser Value
jsonParser = do
    space
    v <- valueParser
    eof
    pure v

-- | Parse a JSON value
valueParser :: Parser Value
valueParser =  do 
    v <-       (objVParser   <?> "object")
           <|> (strVParser   <?> "string")
           <|> (arrayVParser <?> "array")                             
           <|> (numVParser   <?> "number")
           <|> (boolVParser  <?> "boolean")
           <|> (nullVParser  <?> "null")
    space
    pure v

-- | Parse an object.
objectParser :: Parser [Field]
objectParser = do 
    -- Duplicate labels only affect the labels of the outer most object, and not
    -- nested objects. Thus, we reset the set of existing labels when we start
    -- parsing a new object.   
    lift $ put [] 
    bracesParser fieldsParser

-- | Parse a list of fields found in an object.
fieldsParser :: Parser [Field]
fieldsParser = sepBy fieldParser commaTok

-- | Parse a field of an object.
fieldParser :: Parser Field
fieldParser = do
    l <- fieldLabelParser
    existingLabels <- get
    -- Is `l` a duplicate field?
    if l `elem` existingLabels
    then customFailure $ JTPEDuplicateField l
    else do skip colonTok
            v <- valueTUParser
            -- Add `l` to the set of existing labels.
            put $ l:existingLabels
            pure $ (l,v)

-- | Parse a field label found in a field of an object.
fieldLabelParser :: Parser DT.Text
fieldLabelParser = doubleQuotedParser charsParser

-- | Parse an object value of a field of an object.
objVParser :: Parser Value
objVParser = ObjV <$> objectParser

-- | Parse a string value of a field of an object.
strVParser :: Parser Value
strVParser = StrV <$> doubleQuotedParser charsParser

-- | Parse a number value of a field of an object.
numVParser :: Parser Value
numVParser = do
    -- Try to lookahead up until any decimal point, then we can check for
    -- leading zeros.
    c <- try $ lookAhead $ takeWhileP Nothing isDigit
    dt <- try signedFloat <|> signedDecimal
    case c of 
        -- Check for leading zeros.
        ('0' DT.:< d DT.:< _) | isDigit d -> customFailure JTPELeadingZeros
        _ -> pure $ NumV dt
    where
        signedDecimal :: Parser Double
        signedDecimal = signed space decimal

        signedFloat :: Parser Double
        signedFloat = signed space float

-- | Parse a boolean value of a field of an object.
boolVParser :: Parser Value
boolVParser = do
    dt <- trueTok <|> falseTok
    pure . BoolV $ case dt of
                    "true" -> True
                    "false" -> False
                    _ -> error "boolVParser: impossible branch"

-- | Parse a null value of a field of an object.
nullVParser :: Parser Value
nullVParser =  nullTok
            *> pure NullV

-- | Parse a `Value` or `Template`.
valueTUParser :: Parser (TU Value)
valueTUParser = StrT.parseTU templateParser valueParser

-- | Parse an array value of a field of an object.
arrayVParser :: Parser Value
arrayVParser = do 
    ary <- bracketsParser $ flip sepBy commaTok valueTUParser
    pure $ ArrayV ary

-- * Parser combinators
templateParser :: Parser TemplateExp
templateParser = do
    s <- singleQuotedParser charsParser
    case StrT.parseTemplate s of
        Left err -> fail . DT.unpack $ err
        Right t -> pure $ t

-- | Parse a single-quoted output of the input parser.
singleQuotedParser :: Parser a -> Parser a
singleQuotedParser = between (string "'") (tok "'")

-- | Parse a double-quoted output of the input parser.
doubleQuotedParser :: Parser a -> Parser a
doubleQuotedParser = between (string "\"") (tok "\"")

-- | Parse a braced output of the input parser.
bracesParser :: Parser a -> Parser a
bracesParser = between (tok "{") (tok "}")

-- | Parse a bracketed output of the input parser.
bracketsParser :: Parser a -> Parser a
bracketsParser = between (tok "[") (tok "]")

-- | Parse an escape character. 
-- These are one of
-- @['\\','/','"','\'','b','n','f','r','t']@
escapeParser :: Parser Char
escapeParser = do
    skip backslashTok
    escapeCharParser
        <|> unicodeEscapeParser

escapeCharParser :: Parser Char
escapeCharParser = do
    e <- satisfy isEscapeChar
    case escapeToChar e of
        Just c -> pure c
        Nothing -> customFailure JTPEInvalidEscapeChar

-- | Predicate defining JSON escape characters.
isEscapeChar :: Char -> Bool
isEscapeChar = (`elem` ['/','\\','"','\'','b','n','f','r','t'])

escapeToChar :: Char -> Maybe Char
escapeToChar 'b' = Just '\b'
escapeToChar 'n' = Just '\n'
escapeToChar 'f' = Just '\f'
escapeToChar 'r' = Just '\r'
escapeToChar 't' = Just '\t'
escapeToChar '\\' = Just '\\'
escapeToChar '/' = Just '/'
escapeToChar '\'' = Just '\''
escapeToChar '"'  = Just '"'
escapeToChar _    = Nothing

-- | Parse a unicode hex string of the form @uXXXX@ into the hex string
-- @0xXXXX@.
hexCodeParser :: Parser String
hexCodeParser = do
    skip "u"
    d1 <- satisfy isHexDigit
    d2 <- satisfy isHexDigit
    d3 <- satisfy isHexDigit
    d4 <- satisfy isHexDigit
    pure $ "0x" <> [d1,d2,d3,d4]

-- |  Parse a unicode escape character of the form @\uXXXX@. This does handle
-- surrogate pairs. 
unicodeEscapeParser :: Parser Char
unicodeEscapeParser = do
    code1 <- hexCodeParser
    let i  = read code1 :: Int
    if i >= 0xD800 && i <= 0xDBFF
    then do -- Parsed high
            skip backslashTok
            code2 <- hexCodeParser    
            let j  = read code2 :: Int
            if j >= 0xDC00 && j <= 0xDFFF
            then do -- Parsed low            
                    let c = 0x10000 + (i - 0xD800) * 0x400 + (j - 0xDC00)   
                    pure . chr $ c
            else customFailure . JTPEUnicode $ "expected a low surrogate"
    else if i >= 0xDC00 && i <= 0xDFFF
         then customFailure . JTPEUnicode $ "lone low surrogate"
         else -- BMP character
              pure . chr $ i

-- | Parse a single unicode character including escapes.
charParser :: Parser Char
charParser = choice [
        satisfy (\c -> not (c `elem` ['\\','"','\'']) && isPrint c),
        escapeParser
    ]

-- | Parse as many unicode characters as possible including escaped characters.
charsParser :: Parser DT.Text
charsParser = DT.pack <$> many charParser 

-- * Tokens

-- | Parse a token (unicode character)
-- Consumes whitespace *after* the parsed token.
tok :: Tok -> Parser Tok
tok = symbol space

-- | Parse and throw away the symbol parsed by the input token
skip :: Parser Tok -> Parser ()
skip = skipCount 1

-- | Parse the comma token. Consumes whitespace after the parsed token.
commaTok :: Parser Tok
commaTok = tok ","

-- | Parse the "true" token. Consumes whitespace after the parsed token.
trueTok :: Parser Tok
trueTok = tok "true"

-- | Parse the "false" token. Consumes whitespace after the parsed token.
falseTok :: Parser Tok
falseTok = tok "false"

-- | Parse the "null" token. Consumes whitespace after the parsed token.
nullTok :: Parser Tok
nullTok = tok "null"

-- | Parse the colon token. Consumes whitespace after the parsed token.
colonTok :: Parser Tok
colonTok = tok ":"

-- | Parse the backslash token.
backslashTok :: Parser Tok
backslashTok = string "\\"
