{-# LANGUAGE FlexibleInstances, LambdaCase #-}

module Alg where

import Syntax
import Data.List (find, delete, union)
import Data.Maybe (fromJust)

-- Algorithmic Chain
data Chain =
    Sub Typ Typ
  | Chk Exp Typ
  | Inf Exp (Typ -> Chain)
  | AInf Typ Exp (Typ -> Chain)
  | ATyp Typ Typ (Typ -> Chain)
  | LChk Exp Typ Typ
  | LInf Typ Exp Exp (Typ -> Chain)
  | End

instance Show Chain where
  show c = show' c 0
    where
      show' (Sub a b) _ = show a ++ " <: " ++ show b
      show' (Chk e t) _ = show e ++ " <== " ++ show t
      show' (Inf e c) n = show e ++ " ==>" ++ show n ++ " " ++
                            show' (c (TVar $ show n)) (n + 1)
      show' (AInf a e c) n = show a ++ " * " ++ show e ++ " =>>" ++ show n ++ " " ++
                            show' (c (TVar $ show n)) (n + 1)
      show' (ATyp a b c) n = show a ++ " o " ++ show b ++ " =>>" ++ show n ++ " " ++
                            show' (c (TVar $ show n)) (n + 1)
      show' (LChk e a b) n = "" ++ show e ++ " <=={" ++ show a ++ " :: List} " ++ show b
      show' (LInf a e e1 c) n = show a ++ " # " ++ show e ++ " # " ++ show e1 ++ " =>>[]" ++ show n ++ " " ++
                            show' (c (TVar $ show n)) (n + 1)
      show' End _ = "End"

-- Worklist
type Worklist = [Work]
data Work =
    WTVar String
  | WSVar String
  | WEVar String
  | WBind String Typ
  | WJug Chain
  deriving Show

instance {-# OVERLAPPING #-} Show [Work] where
  show [] = "."
  show (WTVar x : w) = show w ++ ", " ++ x
  show (WSVar x : w) = show w ++ ", ~" ++ x
  show (WEVar x : w) = show w ++ ", ^" ++ x
  show (WBind x t : w) = show w ++ ", " ++ x ++ " : " ++ show t
  show (WJug c : w) = show w ++ " ||- " ++ show c

eesubst :: String -> Exp -> Exp -> Exp
eesubst s e (Lam x b)
  | s == x    = Lam x b
  | otherwise = Lam x (eesubst s e b)
eesubst s e (App e1 e2) = App (eesubst s e e1) (eesubst s e e2)
eesubst s e (Ann e1 t) = Ann (eesubst s e e1) t
eesubst s e (TApp e1 t) = TApp (eesubst s e e1) t
eesubst s e (TAbs x e1) = TAbs x (eesubst s e e1)
eesubst s e (Var x)
  | s == x    = e
  | otherwise = Var x
eesubst s e (Cons e1 e2) = Cons (eesubst s e e1) (eesubst s e e2)
eesubst s e (Case e1 e2 e3) = Case (eesubst s e e1) (eesubst s e e2) (eesubst s e e3)
eesubst s e (Fix e1) = Fix (eesubst s e e1)
eesubst s e (Let x e1 e2)
  | s == x    = Let x e1 e2
  | otherwise = Let x (eesubst s e e1) (eesubst s e e2)
eesubst s e t = t

esubst :: String -> Typ -> Exp -> Exp
esubst e s (Lam x b) = Lam x (esubst e s b)
esubst e s (App e1 e2) = App (esubst e s e1) (esubst e s e2)
esubst e s (Ann e1 t) = Ann (esubst e s e1) (tsubst e s t)
esubst e s (TApp e1 t) = TApp (esubst e s e1) (tsubst e s t)
esubst e s (TAbs x e1) = TAbs x (esubst e s e1)
esubst e s (Cons e1 e2) = Cons (esubst e s e1) (esubst e s e2)
esubst e s (Case e1 e2 e3) = Case (esubst e s e1) (esubst e s e2) (esubst e s e3)
esubst e s (Fix e1) = Fix (esubst e s e1)
esubst e s (Let x e1 e2) = Let x (esubst e s e1) (esubst e s e2)
esubst e s t = t

etsubst :: String -> Typ -> Exp -> Exp
etsubst e s (Lam x b) = Lam x (etsubst e s b)
etsubst e s (App e1 e2) = App (etsubst e s e1) (etsubst e s e2)
etsubst e s (Ann e1 t) = Ann (etsubst e s e1) (ttsubst e s t)
etsubst e s (TApp e1 t) = TApp (etsubst e s e1) (ttsubst e s t)
etsubst e s (TAbs x e1) = TAbs x (etsubst e s e1)
etsubst e s (Cons e1 e2) = Cons (etsubst e s e1) (etsubst e s e2)
etsubst e s (Case e1 e2 e3) = Case (etsubst e s e1) (etsubst e s e2) (etsubst e s e3)
etsubst e s (Fix e1) = Fix (etsubst e s e1)
etsubst e s (Let x e1 e2) = Let x (etsubst e s e1) (etsubst e s e2)
etsubst e s t = t

tsubst :: String -> Typ -> Typ -> Typ
tsubst s t TInt = TInt
tsubst s t TBool = TBool
tsubst s t TTop = TTop
tsubst s t TBot = TBot
tsubst s t (TVar a) = TVar a
tsubst s t (SVar a) = SVar a
tsubst s t (EVar x)
  | s == x     = t
  | otherwise  = EVar x
tsubst s t (Forall a b)
  | s == a     = Forall a b
  | otherwise  = Forall a (tsubst s t b)
tsubst s t (TArr t1 t2) =
  TArr (tsubst s t t1) (tsubst s t t2)
tsubst s t (TList t1) = TList (tsubst s t t1)

ttsubst :: String -> Typ -> Typ -> Typ
ttsubst s t TInt = TInt
ttsubst s t TBool = TBool
ttsubst s t TTop = TTop
ttsubst s t TBot = TBot
ttsubst s t (EVar a) = EVar a
ttsubst s t (SVar a) = SVar a
ttsubst s t (TVar x)
  | s == x     = t
  | otherwise  = TVar x
ttsubst s t (Forall a b)
  | s == a     = Forall a b
  | otherwise  = Forall a (ttsubst s t b)
ttsubst s t (TArr t1 t2) =
  TArr (ttsubst s t t1) (ttsubst s t t2)
ttsubst s t (TList t1) = TList (ttsubst s t t1)

csubst :: String -> Typ -> Chain -> Chain
csubst s t (Sub a b) = Sub (tsubst s t a) (tsubst s t b)
csubst s t (Chk e a) = Chk (esubst s t e) (tsubst s t a)
csubst s t (Inf e f) = Inf (esubst s t e) (csubst s t . f)
csubst s t (AInf t1 e f) = AInf (tsubst s t t1) (esubst s t e) (csubst s t . f)
csubst s t (ATyp t1 t2 f) = ATyp (tsubst s t t1) (tsubst s t t2) (csubst s t . f)
csubst s t (LChk e a b) = LChk (esubst s t e) (tsubst s t a) (tsubst s t b)
csubst s t (LInf t1 e e1 f) = LInf (tsubst s t t1) (esubst s t e) (esubst s t e1) (csubst s t . f)
csubst s t End = End

-- substitute a existential variable by its solution and replace its declaration by xs
wsubst :: String -> [String] -> Typ -> Worklist -> Worklist
wsubst s xs t = concatMap wsubst1
  where
    wsubst1 (WTVar x)   = [WTVar x]
    wsubst1 (WSVar x)   = [WSVar x]
    wsubst1 (WEVar x)
      | s == x    = reverse $ map WEVar xs
      | otherwise = [WEVar x]
    wsubst1 (WBind x a) = [WBind x (tsubst s t a)]
    wsubst1 (WJug c)    = [WJug (csubst s t c)]

ftv :: Typ -> [Typ]
ftv TInt = []
ftv TBool = []
ftv TTop = []
ftv TBot = []
ftv (TVar a) = [TVar a]
ftv (SVar a) = [SVar a]
ftv (EVar x) = [EVar x]
ftv (Forall a b) = delete (TVar a) (ftv b)
ftv (TArr t1 t2) = ftv t1 `union` ftv t2
ftv (TList t1) = ftv t1

-- Is a declared before b?
prec :: Worklist -> String -> String -> Bool
prec w a b = elem a . dropWhile (/= b) $ wex
  where
    wex = concatMap (\case
        WEVar v -> [v]
        WTVar v -> [v]
        _       -> []
      ) w

genSplit :: [Char] -> ([Char], [Char])
genSplit x = (x ++ "1", x ++ "2")

findBind :: String -> Worklist -> Maybe Typ
findBind x (WBind y a : w)
  | x == y    = Just a
  | otherwise = findBind x w
findBind x (_ : w) = findBind x w
findBind _ [] = Nothing

pickNewVar w = [fromJust $ find (\c -> all (\var -> c `notElem` var) wvars) ['a'..'w']]
  where
    wvars = concatMap (\case
        WTVar v -> [v]
        WSVar v -> [v]
        WEVar v -> [v]
        _ -> []
      ) w

expVar :: Exp -> [String]
expVar (Var x) = [x]
expVar (Lam x b) = x : expVar b
expVar (App e1 e2) = expVar e1 ++ expVar e2
expVar (Cons e1 e2) = expVar e1 ++ expVar e2
expVar (Case e1 e2 e3) = expVar e1 ++ expVar e2 ++ expVar e3
expVar (Fix e) = expVar e
expVar _ = []

pickNewBindVar e w = fromJust $ find (`notElem` wvars) bvarsupply
  where
    wvars = concatMap (\case
        WBind v _ -> [v]
        _ -> []
      ) w ++ expVar e
    bvarsupply = "x" : "y" : [ xy : show n | n <- [1..100], xy <- ['x', 'y'] ]

step :: Worklist -> Worklist
-- First 4 rules
step (WTVar a : w) = w
step (WEVar a : w) = w
step (WSVar a : w) = w
step (WBind x t : w) = w

-- Subtyping --
-- Rules 5 to 9 and 13, 16 and 17
step (WJug (Sub TInt TInt) : w) = w
step (WJug (Sub TBool TBool) : w) = w
step (WJug (Sub (TVar a) (TVar b)) : w)
  | a == b    = w
  | otherwise = error "6"
step (WJug (Sub (SVar a) (SVar b)) : w)
  | a == b    = w
  | otherwise = error "7"
step (WJug (Sub (EVar a) (EVar b)) : w)
  | a == b     = w
  | prec w a b = wsubst b [] (EVar a) w -- rule 16
  | otherwise  = wsubst a [] (EVar b) w -- rule 17
-- Rules 18 to 21
step (WJug (Sub (TVar a) (EVar b)) : w)
  | prec w a b = wsubst b [] (TVar a) w
  | otherwise  = error "18"
step (WJug (Sub (EVar b) (TVar a)) : w)
  | prec w a b = wsubst b [] (TVar a) w
  | otherwise  = error "19"
step (WJug (Sub TInt (EVar b)) : w) = wsubst b [] TInt w
step (WJug (Sub (EVar b) TInt) : w) = wsubst b [] TInt w
step (WJug (Sub TBool (EVar b)) : w) = wsubst b [] TBool w
step (WJug (Sub (EVar b) TBool) : w) = wsubst b [] TBool w

-- rule 8 and 9
step (WJug (Sub _ TTop) : w) = w
step (WJug (Sub TBot _) : w) = w
-- rule 10
step (WJug (Sub (TArr a b) (TArr c d)) : w) = WJug (Sub c a) : WJug (Sub b d) : w
-- rules 12 and 11
step (WJug (Sub (Forall a t1) (Forall b t2)) : w) = WJug (Sub t1' t2') : WSVar x : w
    where
      x = pickNewVar w
      t1' = ttsubst a (SVar x) t1
      t2' = ttsubst b (SVar x) t2
step (WJug (Sub (Forall a t1) t2) : w) = WJug (Sub t' t2) : WEVar x : w
    where
      x = pickNewVar w
      t' = ttsubst a (EVar x) t1
-- rules 14 and 15
step (WJug (Sub (EVar a) (TArr b c)) : w)
  | EVar a `notElem` ftv (TArr b c) = let (a1, a2) = genSplit a
      in wsubst a [a1, a2] (TArr (EVar a1) (EVar a2)) $ WJug (Sub (EVar a) (TArr b c)) : w
  | otherwise = error "10"
step (WJug (Sub (TArr b c) (EVar a)) : w)
  | EVar a `notElem` ftv (TArr b c) = let (a1, a2) = genSplit a
      in wsubst a [a1, a2] (TArr (EVar a1) (EVar a2)) $ WJug (Sub (TArr b c) (EVar a)) : w
  | otherwise = error "11"
-- list rules
step (WJug (Sub (TList a) (TList b)) : w) = WJug (Sub a b) : w
step (WJug (Sub (EVar a) (TList b)) : w) = wsubst a [x] (TList (EVar x)) $ WJug (Sub (EVar x) b) : w
    where x = pickNewVar w
step (WJug (Sub (TList b) (EVar a)) : w) = wsubst a [x] (TList (EVar x)) $ WJug (Sub b (EVar x)) : w
    where x = pickNewVar w

-- Checking --
-- rules 23 to 26
step (WJug (Chk (Lam x e) (Forall a t)) : w) = WJug (Chk (Lam x e) t') : WTVar b : w
    where
      b = pickNewVar w
      t' = ttsubst a (TVar b) t
step (WJug (Chk (Lam x e) (TArr a b)) : w) = WJug (Chk e' b) : WBind y a : w
    where
      y = pickNewBindVar e w
      e' = eesubst x (Var y) e
step (WJug (Chk (Lam x e) (EVar a)) : w) = let (a1, a2) = genSplit a
      in wsubst a [a1, a2] (TArr (EVar a1) (EVar a2)) $
        WJug (Chk e' (EVar a2)) : WBind y (EVar a1) : w
    where
      y = pickNewBindVar e w
      e' = eesubst x (Var y) e
step (WJug (Chk e TTop) : w) = w
-- type abstraction (new)
step (WJug (Chk (TAbs a e) (Forall b t)) : w)
  | a == b = WJug (Chk e' t') : WTVar c : w
  | otherwise = error "typ abs"
    where
      c = pickNewVar w
      e' = etsubst a (TVar c) e
      t' = ttsubst a (TVar c) t
-- list rules
step (WJug (Chk Nil (TList a)) : w) = w
step (WJug (Chk (Cons e1 e2) (TList a)) : w) = WJug (Chk e1 a) : WJug (Chk e2 (TList a)) : w
step (WJug (LChk e (TList a) b) : w) = WJug (Chk e (TArr a (TArr (TList a) b))) : w
step (WJug (Chk (Case e e1 e2) b) : w) = WJug (Chk e1 b) : WJug (Inf e (\a -> LChk e2 a b)) : w
-- fix rule
step (WJug (Chk (Fix e) a) : w) = WJug (Chk e (TArr a a)) : w
-- let rule
step (WJug (Chk (LetA x t e1 e2) b) : w) = WJug (Chk (App (Ann (Lam x e2) (TArr t b)) (Ann (Fix (Lam x e1)) t)) b) : w
-- rule 22
step (WJug (Chk e b) : w) = WJug (Inf e (`Sub` b)) : w

-- Inference --
step (WJug (Inf (Var x) c) : w) = case findBind x w of
    Just a  -> WJug (c a) : w
    Nothing -> error $ "No binding for " ++ x
step (WJug (Inf (Ann e a) c) : w) = WJug (Chk e a) : WJug (c a) : w
step (WJug (Inf (ILit _) c) : w) = WJug (c TInt) : w
step (WJug (Inf (BLit _) c) : w) = WJug (c TBool) : w
step (WJug (Inf (Lam x e) c) : w) = WJug (Chk e' (EVar b)) : WBind y (EVar a) :
      WJug (c (TArr (EVar a) (EVar b))) : WEVar b : WEVar a : w
    where
      a = pickNewVar w
      b = pickNewVar (WEVar a : w)
      y = pickNewBindVar e w
      e' = eesubst x (Var y) e
step (WJug (Inf (App e1 e2) c) : w) = WJug (Inf e1 (\b -> AInf b e2 c)) : w
step (WJug (Inf (TApp e a) c) : w) = WJug (Inf e (\b -> ATyp b a c)) : w

step (WJug (ATyp (Forall a t1) t2 c) : w) = WJug (c (ttsubst a t2 t1)) : w
step (WJug (ATyp TBot t c) : w) = WJug (c TBot) : w

-- Application Inference --
step (WJug (AInf (Forall a t) e c) : w) = WJug (AInf t' e c) : WEVar x : w
    where
      x = pickNewVar w
      t' = ttsubst a (EVar x) t
step (WJug (AInf (TArr a b) e c) : w) = WJug (Chk e a) : WJug (c b) : w
step (WJug (AInf TBot _ c) : w) = WJug (c TBot) : w
step (WJug (AInf (EVar a) e c) : w) = let (a1, a2) = genSplit a
      in wsubst a [a1, a2] (TArr (EVar a1) (EVar a2)) $ WJug (AInf (EVar a) e c) : w

-- list rules
step (WJug (Inf Nil c) : w) = WJug (c (Forall x (TList (TVar x)))) : w
    where x = pickNewVar w
step (WJug (Inf (Cons e1 e2) c) : w) = WJug (Inf e1 $ Chk e2 . TList) : w
step (WJug (Inf (Case e e1 e2) c) : w) = WJug (Inf e1 (\b -> LInf b e e2 c)) : w
step (WJug (LInf b e e2 c) : w) = WJug (Inf e (\a -> LChk e2 a b)) : WJug (c b) : w
-- fix
step (WJug (Inf (Fix e) c) : w) = WJug (Chk e (TArr (EVar a) (EVar a))) : WJug (c (EVar a)) : WEVar a : w
    where a = pickNewVar w
-- let
step (WJug (Inf (Let x e1 e2) c) : w) = WJug (Inf (App (Lam x e2) (Fix (Lam x e1))) c) : w
step (WJug (Inf (LetA x t e1 e2) c) : w) = WJug (Inf e2 c) : WJug (Chk e1 t) : WBind x t : w
-- ret
step (WJug End : w) = w
step w = error $ "No rule matching " ++ show w

runStep :: Worklist -> IO ()
runStep [] = putStrLn "Done."
runStep w = do
  print w
  runStep (step w)
