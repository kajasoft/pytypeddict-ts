module Py2Ts.PyParse where
import Py2Ts.Types (PyType(..))
import           Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer  as L
import           Data.Void (Void)
import Control.Monad (void)
import Data.Text (Text, pack)
import Data.List.NonEmpty (NonEmpty(..))
import Control.Arrow (left)
import Data.Char (isLetter, isAlphaNum)

-- TODO
--- parse and throw away python comments

-- https://markkarpov.com/tutorial/megaparsec.html

type Parser = Parsec Void Text

parseTypes :: Text -> Either String [PyType]
parseTypes s =
      parse p s
    where p = scn >> many (pType <* scn)


parse :: Parser a -> Text -> Either String a
parse p s = left errorBundlePretty $ Text.Megaparsec.parse p "" s


pType :: Parser PyType
pType =
    pUnionBars <|> pTypeNoUnionBars

pTypeNoUnionBars :: Parser PyType
pTypeNoUnionBars =
    pNone
    <|> pStr
    <|> pInt
    <|> pFloat
    <|> pBool
    <|> pDict
    <|> pList
    <|> pTuple
    <|> pDict
    <|> pNotRequired
    <|> pUnionExplicit
    <|> pLiteralStrings
    <|> try pTypedDict
    -- TODO parse more literal stuff
    <|> try pTypeAlias
    <|> pCustomType


-- TODO ignore Comments? or parse and emit them in the other lang


pNone :: Parser PyType
pNone = lexeme (string "None") *> pure PyNone

pStr :: Parser PyType
pStr = lexeme (string "str") *> pure PyStr

pInt :: Parser PyType
pInt = lexeme (string "int") *> pure PyInt

pFloat :: Parser PyType
pFloat = lexeme (string "float") *> pure PyFloat

pBool :: Parser PyType
pBool = lexeme (string "bool") *> pure PyBool

pDict :: Parser PyType
pDict = do
    tys <- pTypeParams "dict"
    case tys of
      k :| (v: []) -> pure $ PyDict k v
      _ -> fail $ "expecting a pair, got: " ++ show tys

pNotRequired :: Parser PyType
pNotRequired = PyNotRequired <$> pTypeParam "NotRequired"

pUnionBars :: Parser PyType
pUnionBars =
   try $ do
     ty:tys <- pTypeNoUnionBars `sepBy1` (lexeme (char '|'))
     case tys of
        _:_ -> pure $ PyUnion (ty :| tys)
        [] -> fail $ "expecting at least 2 elements, got ony 1"

pUnionExplicit :: Parser PyType
pUnionExplicit =
    -- TODO ensure at least two!
    PyUnion <$> pTypeParams "Union"

pOptional :: Parser PyType
pOptional = do
  ty <- pTypeParam "Optional"
  pure $ PyUnion (ty :| [PyNone])


pList :: Parser PyType
pList = PyList <$> (pTypeParam "list" <|> pTypeParam "List")

pTuple :: Parser PyType
pTuple = PyTuple <$> pTypeParams "tuple"

-- indentBlock
-- https://markkarpov.com/tutorial/megaparsec.html
pTypedDict :: Parser PyType
pTypedDict = do
      L.nonIndented scn $ L.indentBlock scn p
    where
      p = do
          _ <- lexeme (string "class")
          tyName <- lexeme typName
          _ <- lexeme (char '(')
          superCls <- lexeme typName
          _ <- lexeme (char ')')
          _ <- lexeme (char ':')
          return $ L.IndentMany Nothing (pure . PyTypedDict tyName superCls) pTypedDictField

pTypedDictField :: Parser (Text, PyType)
pTypedDictField = do
    name <- fieldName
    _ <- lexeme (char ':')
    ty <- pType
    pure $ (name, ty)

-- TODO? "foo" | "bar" etc?
pLiteralStrings :: Parser PyType
pLiteralStrings = pParam "Literal" (pNonEmptyListOf primString) PyLiteral



pTypeAlias :: Parser PyType
pTypeAlias = do
  name <- lexeme typName
  _ <- lexeme (char '=')
  typ <- pType
  pure $ PyTypeAlias name typ



-- | A custom type, e.g. a TypedDict name.
-- Custom type cannot be parameterized. This is a catch-all fallback parser.
pCustomType :: Parser PyType
pCustomType =
  (PyCustomType <$> typName)
  <|>
  (PyCustomType <$> quoted typName)


-- not sure if field names need to be lowercase, just take both cases
fieldName :: Parser Text
fieldName =
   pack <$>
    lexeme
      ((:) <$> letterCharOrUnderscore <*> many alphaNumCharOrUnderscore <?> "fieldName")

letterCharOrUnderscore :: Parser Char
letterCharOrUnderscore =
    satisfy (\c -> isLetter c || c == '_')


alphaNumCharOrUnderscore :: Parser Char
alphaNumCharOrUnderscore =
    satisfy (\c -> isAlphaNum c || c == '_')



-- not sure if type/class names need to be upper, just take both cases
typName :: Parser Text
typName = fieldName

primString :: Parser Text
primString = pack <$> parseString


pParam :: Text -> Parser a -> (a -> b) -> Parser b
pParam typLabel paramParser ctr = do
    _ <- lexeme (string typLabel)
    _ <- lexemeNL (char '[')
    param <- paramParser <* scn
    _ <- (lexemeNL (char ']'))
    pure $ ctr param

pTypeParam :: Text -> Parser PyType
pTypeParam typLabel = pParam typLabel pType id

pTypeParams :: Text -> Parser (NonEmpty PyType)
pTypeParams typLabel = pParam typLabel (pNonEmptyListOf pType) id

-- accounts for optional trailing comma
pNonEmptyListOf :: Parser a -> Parser (NonEmpty a)
pNonEmptyListOf p = try $ do
    ty:tys <- p `sepEndBy1` (lexemeNL (sc *> char ','))
    pure $ ty :| tys





-- simple parser transformer to parse within single or double quotes
quoted :: Parser a -> Parser a
quoted p =
    (between (char '\'') (char '\'') p)
    <|>
    (between (char '"') (char '"') p)


-- Quoted String
-- https://stackoverflow.com/questions/24106314/parser-for-quoted-string-using-parsec

parseString :: Parser String
parseString =
    parseStringBetween '"' '"'
    <|> parseStringBetween '\'' '\''

parseStringBetween :: Char -> Char -> Parser String
parseStringBetween openChar closeChar = do
    char openChar
    strings <- many (character closeChar)
    char closeChar
    return $ concat strings

character :: Char -> Parser String
character closeChar = fmap return (nonEscape closeChar) <|> escape

escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf ("\\\"0nrvtbf" :: String) -- all the characters which can be escaped
    return [d, c]

nonEscape :: Char -> Parser Char
nonEscape closeChar =
  noneOf (closeChar:"\\\0\n\r\v\t\b\f")



-- lexeme picks up all the trailing white space using the supplied
-- space consumer
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- newline consuming lexeme
lexemeNL :: Parser a -> Parser a
lexemeNL = L.lexeme scn

-- space consumer that consumes newlines
scn :: Parser ()
scn = L.space space1 (L.skipLineComment "#") empty

-- space consumer that does not consume newlines
sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t'))
             (L.skipLineComment "#") -- python comments
             empty


{-|

TimeStamp = int

class MeilisearchTitleDict(TypedDict):
    actors: list[str]
    age: int | None
    alpha_title: str
    content_type: ContentType
    countries: list[str]
    episode_runtimes: NotRequired[list[int]]
    episodes: list['MeilisearchTitleDict']
    first_air_year: NotRequired[int | None]
    genre_names: list[int]
    id: int
    image_lg: str | None
    image_md: str | None
    image_sm: str | None
    imdb_id: str

-}



