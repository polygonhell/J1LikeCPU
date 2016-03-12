{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Stack where


import CLaSH.Prelude
import qualified GHC.Base as B
import Prelude hiding ((!!), head)
import Text.Printf
-- import Debug.Trace


-- N X ...
-- TODO this could just be a Vector with pattern matching
data Stack (n :: Nat) (m :: Nat) = Stack (BitVector m) (BitVector m) (Vec n (BitVector m)) 
stack :: forall n m . (KnownNat n, KnownNat m)  => Stack n m -> (BitVector m, BitVector m, Bit, BitVector 2) -> Stack n m
stack (Stack n x tl) (h, h2, we, delta) = Stack n' x' tl' where
    (n', x') = if we == 1 then (h, h2) else nx'' -- Have to overwrite both leading values
    (nx'', tl') = case delta of
        -1 -> ((x, tl !! (0::Int)), tl <<+ 0)
        -2 -> ((tl !! (0::Int), tl !! (1::Int)), tl <<+ 0 <<+ 0)
        1 ->  ((0, n), x +>> tl)
        _ -> ((n, x), tl)


instance forall n m . (KnownNat n, KnownNat m)  => Show (Stack n m) where
  -- show :: forall n m . (KnownNat n, KnownNat m) => Stack n m -> String
  show a = str where 
    Stack b x tl = a
    str = printf "%08x:%08x:%08x:%08x..." (toInteger b) (toInteger x) (toInteger (tl !! (0::Int)))  (toInteger (tl !! (1::Int)))


stackTests :: IO ()
stackTests = success where
  st1@(Stack h h' _) = Stack 1 2 (3 :> 4 :> 5:> Nil) :: Stack 3 8
  st2 = stack st1 (0, h, 1, 1) 
  st3 = stack st1 (0, 0, 0, -1) 
  st4 = stack st1 (0, 0, 0, -2) 
  st5 = stack st1 (h', h, 1, 0) 
  success = do 
    print "Hello"
    print $ "init     " B.++ show st1
    print $ "push 0 : " B.++ show st2
    print $ "drop :   " B.++ show st3
    print $ "2drop :  " B.++ show st4
    print $ "swap :   " B.++ show st5 

