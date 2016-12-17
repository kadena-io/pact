module SolverAttempt where


import Control.Monad.Reader

type Env = [(String, Int)]
type Eval a = ReaderT Env Maybe a

data Cmp = CGT | CLT deriving Show

data Expr
  = Val Int
  | Add Expr Expr
  | Ite (Cmp,Expr,Expr) Expr Expr
  | Var String
  deriving (Show)

eval :: Expr -> Eval Int
eval ex = case ex of

  Val n -> return n

  Add x y -> do
    a <- eval x
    b <- eval y
    return (a+b)

  Var x -> do
    env <- ask
    val <- lift (lookup x env)
    return val

  Ite (c,l,r) x y -> do
    l' <- eval l
    r' <- eval r
    case c of
      CGT -> if l' > r' then eval x else eval y
      CLT -> if l' < r' then eval x else eval y

env :: Env
env = [("x", 2), ("y", 5)]

ex1, ex2, ex3 :: Expr
ex1 = Add (Val 2) (Add (Val 1) (Var "x"))
ex2 = Add (Val 5) (Add (Val 2) (Var "x"))
ex3 = Ite (CGT, Var "x", Var "y") ex1 ex2

example1, example2, example3 :: Maybe Int
example1 = runReaderT (eval ex1) env
example2 = runReaderT (eval ex2) env
example3 = runReaderT (eval ex3) env
