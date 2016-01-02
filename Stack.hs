{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}

module Stack where


import CLaSH.Prelude
import CLaSH.Sized.Unsigned
import qualified Data.List as L
import Text.Printf
import Debug.Trace

import Types

-- Head Tail Ptr
data Stack (n :: Nat) (m :: Nat) = Stack (BitVector m) (Vec (n) (BitVector m)) deriving (Show)

-- Note delta is really sign + magnitude
stack :: forall n m . (KnownNat n, KnownNat m)  => Stack n m -> (Bit, BitVector 2, BitVector m) -> (Stack n m, BitVector m)
stack (Stack hd tl) (we, delta, wd) = (Stack hd' tl', hd) where  
  hd' = if (we .|. move) == 1 then hd'' else hd
  tl' = if move == 1 then tl'' else tl
  move = delta ! 0
  hd'' = if we == 1 then wd else (tl !! 0)
  tl'' = if (delta ! 1) == 1 then tl <<+ -1 else hd +>> tl




