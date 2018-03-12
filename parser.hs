import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~\\/"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | String String
             | Char String
             | Array [LispVal]
             | Bool Bool

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  c <- oneOf ['\\', '"']
  return c

parseString :: Parser LispVal
parseString = do
  char '"'
  s <- many ((noneOf ['\\', '"']) <|> escapedChars )
  char '"'
  return $ String s

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber =  parseRadixNumber <|> parsePlainNumber

parseRadixNumber :: Parser LispVal
parseRadixNumber = do
  char '#' >>
    (parseDecimal <|> parseHex <|> parseOct )


parsePlainNumber :: Parser LispVal
parsePlainNumber = do
  num <- many1(digit)
  (return . Number . read) num

parseDecimal :: Parser LispVal
parseDecimal = do
  char 'd'
  num <- many(digit)
  (return . Number . read) num

readRadixNum r n t = case r n of
                [(n, _)] -> t n
                _ -> t 0

parseHex :: Parser LispVal
parseHex = do
  char 'h'
  num <- many(hexDigit)
  return $ readRadixNum readHex num Number

parseOct :: Parser LispVal
parseOct = do
  char 'o'
  num <- many(octDigit)
  return $ readRadixNum readOct num Number

parseFloat :: Parser LispVal
parseFloat = do
  whole <- many1 digit
  char '.'
  decimal <- many1 digit
  return $ readRadixNum readFloat (whole ++ "." ++ decimal) Float

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseString
            <|> parseAtom
            <|> try parseFloat
            <|> parseNumber
            <|> parseQuoted
            <|> do char '('
                   x <- try parseList <|> parseDottedList
                   char ')'
                   return x


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match:" ++ show err
  Right val -> "Found value: " ++ show val

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float num) = show num
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List lps) = "(" ++ unwordsList lps ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where
  show = showVal

main :: IO()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
