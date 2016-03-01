{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}

module Stack where


import CLaSH.Prelude
import Prelude hiding ((!!), head)
-- import Debug.Trace


-- Head Tail Ptr
data Stack (n :: Nat) (m :: Nat) = Stack (BitVector m) (Vec n (BitVector m)) deriving (Show)

-- Note delta is really sign + magnitude
stack :: forall n m . (KnownNat n, KnownNat m)  => Stack n m -> (Bit, BitVector 2, BitVector m) -> (Stack n m, BitVector m)
stack (Stack hd tl) (we, delta, wd) = (Stack hd' tl', hd) where  
  hd' = if (we .|. move) == 1 then hd'' else hd
  tl' = if move == 1 then tl'' else tl
  move = delta ! (0 :: Int)
  hd'' = if we == 1 then wd else  tl !! (0 :: Int)
  tl'' = if (delta ! (1::Int)) == 1 then tl <<+ -1 else hd +>> tl


-- N X ...
data Stack2 (n :: Nat) (m :: Nat) = Stack2 (BitVector m) (BitVector m) (Vec n (BitVector m)) deriving (Show)
stack2 :: forall n m . (KnownNat n, KnownNat m)  => Stack2 n m -> (BitVector m, BitVector m, Bit, BitVector 2) -> Stack2 n m
stack2 (Stack2 n x tl) (h, h2, we, delta) = Stack2 n' x' tl' where
    (n', x') = if we == 1 then (h, h2) else nx'' -- Have to overwrite both leading values
    (nx'', tl') = case delta of
        -1 -> ((x, tl !! (0::Int)), tl <<+ 0)
        -2 -> ((tl !! (0::Int), tl !! (1::Int)), tl <<+ 0 <<+ 0)
        1 ->  ((0, n), x +>> tl)
        _ -> ((n, x), tl)




