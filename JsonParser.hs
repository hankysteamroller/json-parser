module JsonParser where

import           Control.Applicative
import           Data.Char

data JsonValue = JsonNull | JsonNumber Int | JsonBoolean Bool | JsonString String | JsonArray [JsonValue] | JsonObject [(String, JsonValue)]
  deriving (Show)

newtype Parser a = Parser { runParser :: (String -> Maybe (String, a)) }

instance Functor Parser where
  fmap f p = Parser (\ s ->
    case runParser p s of
      Just (s', a) -> Just (s', f a)
      _            -> Nothing
    )

instance Applicative Parser where
  pure x = Parser (\ s -> Just (s, x))
  p1 <*> p2 = Parser (\ s ->
    case runParser p1 s of
      Just (s', f) ->
        case runParser p2 s' of
          Just (s'', x) -> Just (s'', f x)
          _             -> Nothing
      _            -> Nothing
    )

instance Alternative Parser where
  empty = Parser (const Nothing)
  p1 <|> p2 = Parser (\ s -> runParser p1 s <|> runParser p2 s)

mkCharP :: Char -> Parser Char
mkCharP c = Parser (\ s ->
  case s of
    (x:xs) | x == c -> Just (xs, x)
    _               -> Nothing
  )

mkStringP :: String -> Parser String
mkStringP = sequenceA . map mkCharP

mkEnclosedByP :: Char -> Parser a -> Char -> Parser a
mkEnclosedByP c1 p c2 = mkCharP c1 *> p <* mkCharP c2

mkSepByP :: Parser a -> Char -> Parser [a]
mkSepByP p sep = Parser (\ s ->
  case runParser p s of
    Just (rest, x) ->
      case runParser (mkCharP sep) rest of
        Just (rest', _) ->
          case runParser (mkSepByP p sep) rest' of
            Just (rest'', xs) -> Just (rest'', x:xs)
            _                 -> Nothing    -- parsing failed after separator
        _               -> Just (rest, [x]) -- there was no separator so the rest is not our problem
    _ -> Nothing
  )

untilDoubleQuoteP :: Parser String
untilDoubleQuoteP = Parser (\ s ->
  case span (/= '"') s of
    (x, rest) | length rest > 0 -> Just (rest, x)
    _                           -> Nothing
  )

wsP :: Parser String
wsP = Parser (\ s ->
  case span isSpace s of
    (_, rest) -> Just (rest, "")
  )

nullP :: Parser JsonValue
nullP = const JsonNull <$> mkStringP "null"

numberP :: Parser JsonValue
numberP = JsonNumber <$> Parser (\ s ->
  case span isDigit s of
    (x, rest) | length x > 0 -> Just (rest, read x)
    _                        -> Nothing
  )

boolP :: Parser JsonValue
boolP = JsonBoolean <$> (\s -> if s == "true" then True else False) <$> (mkStringP "true" <|> mkStringP "false")

stringP :: Parser JsonValue
stringP = JsonString <$> (mkCharP '"' *>  untilDoubleQuoteP <* mkCharP '"')

elementsP :: Parser JsonValue
elementsP = JsonArray <$> (mkSepByP elementP ',' <|> (:[]) <$> elementP)

emptyArrayP :: Parser JsonValue
emptyArrayP = const (JsonArray []) <$> mkEnclosedByP '[' wsP ']'

elementsArrayP :: Parser JsonValue
elementsArrayP = mkEnclosedByP '[' elementsP ']'

arrayP :: Parser JsonValue
arrayP = emptyArrayP <|> elementsArrayP

memberKeyP :: Parser JsonValue
memberKeyP = wsP *> stringP <* wsP

commaP :: Parser Char
commaP = wsP *> mkCharP ',' <* wsP

memberP :: Parser (String, JsonValue)
memberP = Parser (\ s ->
  case runParser memberKeyP s of
    Just (rest, JsonString memberKey) ->
      case runParser (mkCharP ':' *> elementP) rest of
        Just (rest', value) -> Just (rest', (memberKey, value))
        _                   -> Nothing
    _ -> Nothing
  )

membersP :: Parser JsonValue
membersP = JsonObject <$> (mkSepByP memberP ',' <|> (:[]) <$> memberP)

emptyObjectP :: Parser JsonValue
emptyObjectP = const (JsonObject []) <$> mkEnclosedByP '{' wsP '}'

membersObjectP :: Parser JsonValue
membersObjectP = mkEnclosedByP '{' membersP '}'

objectP :: Parser JsonValue
objectP =  emptyObjectP <|> membersObjectP

valueP :: Parser JsonValue
valueP = nullP <|> numberP <|> stringP <|> boolP <|> arrayP <|> objectP

elementP :: Parser JsonValue
elementP = wsP *> valueP <* wsP

jsonP :: Parser JsonValue
jsonP = elementP

parse f = do
  json <- readFile f
  return $ runParser jsonP json
