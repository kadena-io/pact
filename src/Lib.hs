module Lib where

import Z3.Monad
import Control.Monad.Trans (liftIO)

run :: IO ()
run = evalZ3 datatypeScript

-- (set-logic HORN)
-- (declare-fun mc (Int Int) Bool)
--
-- (assert (forall ((m Int)) (=> (> m 100) (mc m (- m 10)))))
-- (assert (forall ((m Int) (n Int) (p Int))
--            (=> (and (<= m 100) (mc (+ m 11) p) (mc p n)) (mc m n))))
--
-- (assert (forall ((m Int) (n Int))
--        (=> (and (mc m n) (<= m 101)) (= n 91))))
-- (check-sat)
-- (get-model)

mkCellDatatype :: Z3 Sort
mkCellDatatype = do
  -- Create a cell data type of the form:
  -- data Cell = Nil | Cons {car :: Cell, cdr :: Cell}

  -- Nil constructor
  nil <- mkStringSymbol "Nil"
  isNil <- mkStringSymbol "is_Nil"
  nilConst <- mkConstructor nil isNil []

  -- Cons constructor
  car <- mkStringSymbol "car"
  cdr <- mkStringSymbol "cdr"
  cons <- mkStringSymbol "Cons"
  isCons <- mkStringSymbol "is_Cons"
  -- In the following, car and cdr are the field names. The second argument,
  -- their sort, is Nothing, since this is a recursive sort. The third argument is
  -- 0, since the type is not mutually recursive.
  consConst <- mkConstructor cons isCons [(car,Nothing,0),(cdr,Nothing,0)]

  -- Cell datatype
  cell <- mkStringSymbol "Cell"
  mkDatatype cell [nilConst, consConst]

datatypeScript :: Z3 ()
datatypeScript = do
  cell <- mkCellDatatype
  liftIO $ putStrLn "Cell constructors are:"
  [nilConst, consConst] <- getDatatypeSortConstructors cell
  mapM_ (\c -> getDeclName c >>= getSymbolString >>= liftIO . putStrLn) [nilConst, consConst]

  nil <- mkApp nilConst []
  -- t1 = Cons (Nil,Nil)
  t1 <- mkApp consConst [nil, nil]

  liftIO $ putStrLn "prove (nil != cons (nil,nil)) //Expect Unsat"
  p <- (mkEq nil t1 >>= mkNot)
  push
  mkNot p >>= assert
  check >>= liftIO . print
  pop 1

  liftIO $ putStrLn "prove (cons (x,u) = cons(y,v) => x = y && u = v) //Expect Unsat"
  [u,v,x,y] <- mapM (flip mkFreshConst cell) ["u","v","x","y"]
  t1 <- mkApp consConst [x,u]
  t2 <- mkApp consConst [y,v]
  p1 <- mkEq t1 t2
  p2 <- mkEq x y
  p3 <- mkEq u v
  p4 <- mkAnd [p2, p3]
  p5 <- mkImplies p1 p4
  push
  mkNot p5 >>= assert
  check >>= liftIO . print
  pop 1
