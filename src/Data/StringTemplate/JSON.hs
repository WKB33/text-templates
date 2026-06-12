{-|
Module      : JSON Templates
Description : String templates for JSON
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@gmail.com

String templates for JSON. The main use of this library is to test JSON
encoders/decoders, but there could be more use cases. This API is designed with
respect to the [ECMA-404 The JSON Data Interchange
Standard](https://www.json.org/json-en.html).
-}
module  Data.StringTemplate.JSON () where
import Text.Megaparsec (Parsec, between, sepBy, skipCount, many, some, satisfy, choice)
import qualified Data.Text as DT
import Data.Void (Void)
import Text.Megaparsec.Char (char)

import Data.StringTemplate
import Data.Char (isPrint)
import Data.StringTemplate.Parser (templateParser)

data FieldValue    = VT Template | VO JSONObject
type Field         = (DT.Text,FieldValue)
newtype JSONObject = JSONObject [Field]

object :: JSONObject -> Template
object (JSONObject fields) = chunk "{" +> fieldsTemplate +> chunk "}"
    where
        fieldsTemplate = foldl (\t f -> comma t +> field f) Empty fields

        comma t@Empty = t
        comma t@_     = t +> chunk ", "

field :: Field -> Template
field (DT.show -> label,VT value) = chunk (label <> ": ") +> value
field (DT.show -> label,VO obj)   = chunk (label <> ": ") +> object obj

type Parser = Parsec Void DT.Text

parseJSONObject :: Parser JSONObject
parseJSONObject = do
    f <- between (char '{') (char '}') parseFields
    pure . JSONObject $ f

parseFields :: Parser [Field]
parseFields = sepBy parseField (char ',')

parseFieldLabel :: Parser DT.Text
parseFieldLabel = parseQuoted parseChars

parseFieldValue :: Parser FieldValue
parseFieldValue = undefined

-- Need to use the witness for the label and value
parseField :: Parser Field
parseField = do
    l <- parseFieldLabel
    parseColon
    v <- parseFieldValue
    pure $ (l,v)

-- * Parser combinators
parseStrTemplate :: Parser Template
parseStrTemplate = templateParser

parseQuoted :: Parser a -> Parser a
parseQuoted p =  (between (some "'")  (some "'")  p)              

parseColon :: Parser ()
parseColon = skipCount 1 $ char ':'

parseEscape :: Parser Char
parseEscape = do
    skipCount 1 (char '\\')
    satisfy (\c -> c == '\\' || c == '"' || c == '\'')

parseChar :: Parser Char
parseChar = choice [
        satisfy (\c -> c /= '\'' && c /= '"' && c /= '\\' && isPrint c),
        parseEscape
    ]

parseChars :: Parser DT.Text
parseChars = DT.pack <$> many parseChar 
