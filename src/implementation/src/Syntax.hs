module Syntax where

-- Algorithmic Types
data Typ =
    TInt
  | TBool
  | TTop
  | TBot
  | TVar String
  | SVar String
  | EVar String
  | Forall String Typ
  | TArr Typ Typ
  | TList Typ
  deriving (Eq)

instance Show Typ where
  show TInt = "Int"
  show TBool = "Bool"
  show TTop = "⊤"
  show TBot = "⊥"
  show (TVar x) = x
  show (EVar x) = "^" ++ x
  show (SVar x) = "~" ++ x
  show (Forall x t) = "∀" ++ x ++ ". " ++ show t
  show (TArr a b) = s ++ " -> " ++ show b
    where
      s = case a of
        Forall _ _ -> showParens $ show a
        TArr _ _ -> showParens $ show a
        _ -> show a
  show (TList t) = "[" ++ show t ++ "]"

-- Expressions
data Exp =
    Var String
  | ILit Integer
  | BLit Bool
  | Lam String Exp
  | App Exp Exp
  | Ann Exp Typ
  | TApp Exp Typ
  | TAbs String Exp
  | Nil
  | Cons Exp Exp
  | Case Exp Exp Exp
  | Fix Exp
  | Let String Exp Exp
  | LetA String Typ Exp Exp
  deriving (Eq)

instance Show Exp where
  show (Var x) = x
  show (ILit n) = show n
  show (BLit n) = show n
  show (Lam x e) = "\\" ++ x ++ " -> " ++ show e
  show (App e1 e2) = showExp e1 ++ " " ++ showExp e2
  show (Ann e t) = showExp e ++ " :: " ++ show t
  show (TApp e t) = showExp e ++ " @ " ++ show t
  show (TAbs x e) = "/\\" ++ x ++ ". " ++ show e
  show Nil = "[]"
  show (Cons e1 e2) = show e1 ++ " : " ++ show e2
  show (Case e e1 e2) = "case " ++ show e ++ " of [] -> " ++ show e1 ++ "; " ++ show e2
  show (Fix e) = "fix " ++ show e
  show (Let x e1 e2) = "let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2
  show (LetA x t e1 e2) = "let " ++ x ++ " :: " ++ show t ++ " = " ++ show e1 ++ " in " ++ show e2
    where
      show' e = case e of
        Lam  {} -> showParens $ show e
        Ann  {} -> showParens $ show e
        TAbs {} -> showParens $ show e
        Cons {} -> showParens $ show e
        Case {} -> showParens $ show e
        Fix  {} -> showParens $ show e
        Let  {} -> showParens $ show e
        LetA {} -> showParens $ show e
        _ -> show e

showParens :: String -> String
showParens s = "(" ++ s ++ ")"

showExp e = case e of
  Lam  {} -> showParens $ show e
  Ann  {} -> showParens $ show e
  TAbs {} -> showParens $ show e
  Cons {} -> showParens $ show e
  Case {} -> showParens $ show e
  Fix  {} -> showParens $ show e
  Let  {} -> showParens $ show e
  LetA {} -> showParens $ show e
  _ -> show e
