module Pugs.Exp where
import Text.PrettyPrint

data Exp
data Stmt

prettyExp :: Exp -> Doc
