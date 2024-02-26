module Main (main) where
import Data.Tree
import Text.XHtml (input)
import Language.Haskell.TH.Syntax (nothingName)
import Distribution.Utils.Json (Json(JsonNull))

data CommandLineValue = Command String | Args [String] | Pipe | And | Or | Files String | InOutType deriving(Show, Eq)

newtype Parser a = Parser {
    runParser :: String -> Maybe (String, a )
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input' , x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    Just (input'', f a)

charP:: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

pAnd:: Parser CommandLineValue
pAnd = And <$ stringP "&&"

commandLineValue :: Parser CommandLineValue
commandLineValue = undefined

main :: IO ()
main = do
  putStrLn "hello world"
