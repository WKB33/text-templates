{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : JSON Templates
Description : String templates for JSON
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@gmail.com

String templates for JSON. The main use of this library is to test JSON
encoders/decoders, but there could be more use cases. This API is designed with
respect to [RFC 8259: STD 90: The JavaScript Object Notation (JSON) Data Interchange Format](https://www.rfc-editor.org/info/rfc8259/).
-}
module  Data.StringTemplate.JSON (parseJSON) where
import Text.Megaparsec (Parsec, between, skipCount, many, satisfy, choice, (<|>), parse, errorBundlePretty, sepBy1, sepBy)
import qualified Data.Text as DT
import Data.Void (Void)
import Text.Megaparsec.Char (string, space)
import Data.Char (isPrint)
import Data.StringTemplate ((+>), chunk, Template, ToTemplate(..) ) 
import Data.StringTemplate qualified as StrT
import Data.StringTemplate.Parser qualified as StrT
import Text.Megaparsec.Char.Lexer (float, symbol)

-- * JSON Syntax

-- | Type of fields of a JSON object.
type Field = (DT.Text,Value)

-- | JSON Value
data Value 
    = TempV  StrT.Template -- ^ String Template
    | ObjV   [Field]       -- ^ Object
    | ArrayV [Value]       -- ^ Array
    | StrV   DT.Text       -- ^ String    
    | NumV   Double        -- ^ Number
    | BoolV  Bool          -- ^ Boolean
    | NullV                -- ^ Null
    deriving Show

instance ToTemplate Value where
    toTemplate :: Value -> Template
    toTemplate = value

instance ToTemplate Field where
    toTemplate :: Field -> Template
    toTemplate = field

-- | Create a template for a JSON value.
value :: Value -> Template
value (TempV  t)   = t
value (ObjV   obj) = object obj
value (ArrayV ary) = array ary
value (StrV   s)   = chunk s
value (NumV   n)   = chunk . DT.show $ n
value (BoolV  b)   = chunk . DT.show $ b
value NullV        = chunk "null"

-- * Creating JSON templates

-- | Create a template from a JSON object.
object :: [Field] -- ^ List of fields of the object
       -> StrT.Template
object fields = StrT.betweenTemplate (chunk "{") (chunk "}") $ StrT.sepTemplatesBy (chunk ", ") fields

--- | Create a template of a field of an object.
field :: Field -- ^ Field of the object
      -> Template
field (DT.show -> label,value) = fieldLabel label +> toTemplate value

-- | Create a template of a field label of a field of an object.
fieldLabel :: DT.Text -- ^ Label of the field
           -> Template
fieldLabel = chunk . (<> ": ") . StrT.doubleQuote

-- | Create a template of an array value.
array :: [Value] -- ^ List of values of the array
      -> Template
array = StrT.bracketTemplate . StrT.sepTemplatesBy (chunk ",")

-- * JSON Templates Parser

-- | Type of tokens.
type Tok    = DT.Text
-- | Type of the parsers that operate on a stream of `Tok`.
type Parser = Parsec Void Tok

-- | The JSON parser.
parseJSON :: DT.Text -- ^ Text to parse
          -> Either DT.Text Value
parseJSON s 
    = case parse valueParser "" s of
        Left bundle -> error $ errorBundlePretty bundle
        Right s -> Right s

-- | Parse a JSON value
valueParser :: Parser Value
valueParser = objVParser
           <|> tempVParser
           <|> strVParser   
           <|> arrayVParser                              
           <|> numVParser
           <|> boolVParser
           <|> nullVParser   

-- | Parse an object.
objectParser :: Parser [Field]
objectParser = bracesParser fieldsParser

-- | Parse a list of fields found in an object.
fieldsParser :: Parser [Field]
fieldsParser =  sepBy1 fieldParser commaTok

-- | Parse a field label found in a field of an object.
fieldLabelParser :: Parser DT.Text
fieldLabelParser = doubleQuotedParser charsParser

-- | Parse a template value of a field of an object.
tempVParser :: Parser Value
tempVParser = TempV <$> templateParser

-- | Parse an object value of a field of an object.
objVParser :: Parser Value
objVParser = ObjV <$> objectParser

-- | Parse a string value of a field of an object.
strVParser :: Parser Value
strVParser = StrV <$> doubleQuotedParser charsParser

-- | Parse a number value of a field of an object.
numVParser :: Parser Value
numVParser = do
    dt <- float
    pure $ NumV dt

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

-- | Parse an array value of a field of an object.
arrayVParser :: Parser Value
arrayVParser = do 
    ary <- bracketsParser $ sepBy valueParser commaTok 
    pure $ ArrayV ary                             

-- | Parse a field of an object.
fieldParser :: Parser Field
fieldParser = do
    l <- fieldLabelParser
    skip colonTok
    v <- valueParser
    pure $ (l,v)

-- * Parser combinators
templateParser :: Parser Template
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
-- @[''\\'','"','\'']@
escapeParser :: Parser Char
escapeParser = do
    skipCount 1 backslashTok
    satisfy (\c -> c == '\\' || c == '"' || c == '\'')

-- | Parse a single unicode character including escapes.
charParser :: Parser Char
charParser = choice [
        satisfy (\c -> c /= '\'' && c /= '"' && c /= '\\' && isPrint c),
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
