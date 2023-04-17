
module Program where
import Exp
import Lab2 ( Parser, endOfInput, whiteSpace, reserved, semiSep1, parseFromFile )
import Parsing ( expr, var, parseFirst, exprParser )
import Sugar ( desugarExp, desugarVar, sugarExp )
import Eval ( substitute )

import Control.Applicative ( Alternative(..) )
import System.IO ( stderr, hPutStrLn, stdout, hFlush )
import qualified Data.Map.Strict as Map
import Data.Map.Strict (fromList, lookup)
import REPLCommand
import Printing (showExp)

data Definition = Definition
  { defHead :: Var
  , defArgs :: [Var]
  , defBody :: ComplexExp
  }
  deriving (Show)

definition :: Parser Definition
definition = do head <- var
                args <- many var
                reserved ":="
                Definition head args <$> expr

-- >>> parseFirst definition "id := \\x -> x"
-- Just (Definition {defHead = Var {getVar = "id"}, defArgs = [], defBody = CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"}))})

-- >>> parseFirst definition "id x := x"
-- Just (Definition {defHead = Var {getVar = "id"}, defArgs = [Var {getVar = "x"}], defBody = CX (Var {getVar = "x"})})

-- >>> parseFirst definition "const x y := x"
-- Just (Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})})

program :: Parser [Definition]
program = whiteSpace *> (semiSep1 definition <* reserved ";") <* endOfInput

-- >>> parseFirst program "    id x := x ; const x y := x"
-- Nothing

-- >>> parseFirst program "    id x := x ; const x y := x ;"
-- Just [Definition {defHead = Var {getVar = "id"}, defArgs = [Var {getVar = "x"}], defBody = CX (Var {getVar = "x"})},Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})}]

definitionExp :: Definition -> ComplexExp
definitionExp def = foldr CLam (defBody def) (defArgs def)

-- >>> definitionExp (Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})})
-- CLam (Var {getVar = "x"}) (CLam (Var {getVar = "y"}) (CX (Var {getVar = "x"})))

type Environment = Map.Map IndexedVar Exp

programEnv :: [Definition] -> Environment
programEnv pgm = fromList (map (\d -> (desugarVar (defHead d), (desugarExp . definitionExp) d )) pgm)

normalizeEnv :: Environment -> Exp -> Exp
normalizeEnv env e = maybe e (normalizeEnv env) (step e)
  where
    step (X x) = Data.Map.Strict.lookup x env
    step (Lam x e) = Lam x <$> step e
    step (App (Lam x ex) e) = Just (substitute x e ex)
    step (App e1 e2)
      = case step e1 of
        Nothing -> App e1 <$> step e2
        Just e1' -> Just (App e1' e2)
