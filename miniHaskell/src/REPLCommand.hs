
module REPLCommand where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef, LanguageDef)
import qualified Text.Parsec.Token as Token
import Text.Parsec (anyChar, string, choice, try, space, eof)
import Control.Applicative (many, Alternative ((<|>)))

data REPLCommand
  = Quit
  | Load String
  | Eval String
  deriving(Show)

replCommand :: Parser REPLCommand
replCommand = choice [quitCommand, loadCommand, evalCommand]
 where
    quitCommand = try (string ":q" <* eof) <|> try (string ":quit" <* eof) >> return Quit
    loadCommand = try (string ":load" <* space) <|> try (string ":l" <* space) >> (Load <$> many anyChar)
    evalCommand = Eval <$> many anyChar

