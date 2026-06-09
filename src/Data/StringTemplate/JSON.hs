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
import Text.Read (Lexeme(String))
import Text.Megaparsec (parseTest,runParserT,Parsec,ParsecT, between, MonadParsec (takeWhile1P, takeWhileP), sepBy, skipCount, many, (<|>), manyTill_, some, satisfy, choice, failure)
import qualified Data.Text as DT
import Data.Void (Void)
import Control.Monad.State (State,lift, get, put, runState)
import Text.Megaparsec.Char (char, asciiChar, printChar)

import Data.StringTemplate.TemplateInternal 
import Data.Char (isPrint)

type ObjectFields = [(Template,JSONTemplate)]
type ArrayFields  = [JSONTemplate]

data JSONLiteral  = TrueLit | FalseLit | NullLit
    deriving Show

data JSONTemplate = ObjectTemplate ObjectFields
                  | ArrayTemplate ArrayFields
                  | StringTemplate Template
                  | NumberTemplate Template
                  | Literal JSONLiteral
    deriving Show

true :: JSONTemplate
true = Literal TrueLit

false :: JSONTemplate
false = Literal FalseLit

null :: JSONTemplate
null = Literal NullLit

object :: ObjectFields -> JSONTemplate
object = ObjectTemplate

array :: ArrayFields -> JSONTemplate
array = ArrayTemplate

string :: Template -> JSONTemplate
string = StringTemplate

number :: Template -> JSONTemplate
number = NumberTemplate

-- Parsing: must parsed based on a witness.

-- Parser's state is a witness template. The witness guides the parsing.
type ParserState = JSONTemplate
type Parser = ParsecT Void DT.Text (State ParserState)

parseJSONTemplate :: Parser JSONTemplate
parseJSONTemplate = undefined

parseObject :: Parser JSONTemplate
parseObject = do
    witness <- lift get
    -- 1: check that the witness matches an object, if not fail
    case witness of
        -- 2: if so, then parse an object out of the stream into a list of fields.
        ObjectTemplate witFields -> do            
            parsedFields <- between (char '{') (char '}') $ parseFields 
            undefined
        _ -> fail ""

parseFields :: Parser [(Template,JSONTemplate)]
parseFields = sepBy parseField (char ',')

parseQuoted :: Parser a -> Parser a
parseQuoted p =  (between (some "'")  (some "'")  p) 
             <|> (between (some "\"") (some "\"") p)

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

parseFieldLabel :: Template -> Parser DT.Text
parseFieldLabel (Chunk label) = do 
    s <- parseQuoted $ parseChars
    if label == s
    then  pure s
    else fail ""
parseFieldLabel (Compose c (i,Nothing) t) = undefined
parseFieldLabel (Compose c (i,Just f) t)  = undefined
parseFieldLabel witLabel = undefined

scanTemplate :: Template -> Parser Template
scanTemplate (Compose c (i,Nothing) t) = do
    void $ string c
    
    undefined

parseFieldValue :: JSONTemplate -> Parser JSONTemplate
parseFieldValue witFieldValue = do
    lift $ put witFieldValue
    parseJSONTemplate

-- Need to use the witness for the label and value
parseField :: (Template,JSONTemplate) -> Parser (Template,JSONTemplate)
parseField (witLabel,witValue) = do
    fieldLabel <- parseFieldLabel witLabel
    parseColon
    fieldValue <- parseFieldValue witValue
    pure $ (undefined,undefined)


