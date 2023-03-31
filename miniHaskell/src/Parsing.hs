module Parsing where

import Exp
import Text.ParserCombinators.Parsec
    ( char, space, string, eof, parse, Parser )
import Text.ParserCombinators.Parsec.Language
    ( haskellStyle, LanguageDef )
import Text.ParserCombinators.Parsec.Token
import Control.Applicative (some)
import Control.Applicative ((<|>), Alternative (many))
import Text.Parsec (try)
import Text.Parsec (choice)

miniHaskellDef :: LanguageDef st
miniHaskellDef = haskellStyle

miniHs :: TokenParser st
miniHs = makeTokenParser miniHaskellDef

testParse :: Parser a -> String -> a
testParse p s
  = case parse p "<input>" s of
      Left err -> error (show err)
      Right a -> a

var :: Parser Var
var = Var <$> (identifier miniHs <|> operator miniHs)
-- >>> testParse var "b is a var"
-- Var {getVar = "b"}

varExp :: Parser ComplexExp
varExp = CX <$> var
-- >>> testParse varExp "b is a var"
-- CX (Var {getVar = "b"})

lambdaExp :: Parser ComplexExp
lambdaExp = do 
             char '\\'
             v <- var 
             string "->"
             space 
             e <- expr 
             return (CLam v e)
-- >>> testParse lambdaExp "\\x -> x"
-- CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"}))

letExp :: Parser ComplexExp
letExp = do 
            string "let"
            space 
            x <- var
            string ":="
            space 
            y <- expr 
            string "in"
            space 
            z <- expr
            return (Let x y z)
-- >>> testParse letExp "let x := y in z"
-- Let (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"}))

letrecExp :: Parser ComplexExp
letrecExp = do 
                string "letrec"
                space 
                x <- var
                string ":="
                space 
                y <- expr 
                string "in"
                space 
                z <- expr
                return (LetRec x y z)
-- >>> testParse letrecExp "letrec x := y in z"
-- LetRec (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"}))

listExp :: Parser ComplexExp
listExp = List <$> (brackets miniHs $ commaSep miniHs expr)


-- >>> testParse listExp "[a,b,c]"
-- List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})]

natExp :: Parser ComplexExp
natExp = Nat <$> (fromInteger <$> natural miniHs)
-- >>> testParse natExp "223 a"
-- Nat 223

parenExp :: Parser ComplexExp
parenExp = parens miniHs expr 
-- >>> testParse parenExp "(a)"
-- CX (Var {getVar = "a"})

basicExp :: Parser ComplexExp
basicExp = choice [try lambdaExp, try letExp, try letrecExp, try listExp, try natExp, try parenExp, try varExp]
-- >>> testParse basicExp "[a,b,c]"
-- List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})]


cappFromList (x : [y]) = CApp x y 
cappFromList (x : xs) = CApp x (cappFromList xs)

capp :: Parser ComplexExp
capp = do 
        f <- basicExp 
        s <- basicExp
        r <- many basicExp
        cappFromList <$> return (f : (s : r))

expr :: Parser ComplexExp
expr = choice [try basicExp, try capp]
-- >>> testParse expr "\\x -> [x,y,z]"
-- CLam (Var {getVar = "x"}) (List [CX (Var {getVar = "x"}),CX (Var {getVar = "y"}),CX (Var {getVar = "z"})])

exprParser :: Parser ComplexExp
exprParser = whiteSpace miniHs *> expr <* eof
-- >>> testParse exprParser "let x := 28 in \\y -> + x y"
-- Let (Var {getVar = "x"}) (Nat 28) (CLam (Var {getVar = "y"}) (CApp (CApp (CX (Var {getVar = "+"})) (CX (Var {getVar = "x"}))) (CX (Var {getVar = "y"}))))
