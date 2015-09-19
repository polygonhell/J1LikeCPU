{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Alu where


import CLaSH.Prelude
import CLaSH.Sized.Unsigned

import qualified Data.List as L
import Text.Printf
import Debug.Trace

import Types


--
-- Left Shift should be performed as an add
--


data AluOp = AluADD
           | AluSUB -- Real 6502 Doesn't have this explicitly - adding it here allows us to not model the separate inverter
           | AluOR
           | AluAND
           | AluXOR
           | AluRSHIFT
           | AluNOP


type VecWord = Vec 32 Bit

-- op aIn bIn cIn (out, carry, overflow)
alu :: AluOp -> WordSize -> WordSize -> Bit -> (WordSize, Bit, Bit)
alu op aIn bIn cIn = case op of
  AluNOP -> (aIn, 0, 0)
  AluOR -> (aIn .|. bIn, 0, 0)
  AluAND -> (aIn .&. bIn, 0, 0)
  AluXOR -> (aIn `xor` bIn, 0, 0)
  AluRSHIFT -> (pack (cIn +>> v), v !! (maxIndex v), 0) where
    v = unpack aIn :: VecWord
  -- Add/Sub case
  _ -> add aIn (complement bIn') cIn where
    bIn' = case op of 
      AluSUB -> complement bIn
      AluADD -> bIn

-- aIn bIn cIn bcd -> (res, cOut, vOut)
add :: WordSize -> WordSize -> Bit -> (WordSize, Bit, Bit)
add aIn bIn cIn = (res, cOut, vOut) where 
  (res, cOut, vOut) = adder cIn aIn bIn

adder :: Bit -> WordSize -> WordSize -> (WordSize, Bit, Bit)
adder cIn xV yV = (pack (reverse sum), cOut, vOut) where
  x = reverse $ unpack xV
  y = reverse $ unpack yV
  res = zipWith3 fullAdder (cIn +>> carries) x y
  (sum, carries) = unzip res
  cOut = carries !! (maxIndex carries)
  vOut = cOut `xor` (carries !! ((maxIndex carries)-1))

fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder cIn x y = (s, cOut) where
  p = x `xor` y
  s = p `xor`cIn
  cOut = if p == low then y else cIn










