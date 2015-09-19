
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK show-extensions #-}


module DPBRam where

import Control.Monad         (when)
import Control.Monad.ST.Lazy (ST,runST)
import Data.Array.MArray     (newListArray,readArray,writeArray)
import Data.Array.ST         (STArray)
import Data.Char             (digitToInt)
import Data.Maybe            (listToMaybe)
import GHC.TypeLits          (KnownNat, type (^))
import Numeric               (readInt)
import System.IO.Unsafe      (unsafePerformIO)

import CLaSH.Promoted.Nat    (SNat,snat,snatToInteger)
import CLaSH.Sized.BitVector (BitVector)
import CLaSH.Signal          (Signal)
import CLaSH.Signal.Explicit (Signal', SClock, register', systemClock)
import CLaSH.Signal.Bundle   (bundle')
import CLaSH.Sized.Unsigned  (Unsigned)

import CLaSH.Prelude.BlockRam.File

import Prelude                     hiding ((++), (!!), concat, drop, foldl,
                                           foldl1, foldr, foldr1, head, init,
                                           iterate, last, length, map, repeat,
                                           replicate, reverse, scanl, scanr,
                                           splitAt, tail, take, unzip, unzip3,
                                           zip, zip3, zipWith, zipWith3)


{-# NOINLINE dpRamFile #-}
dpRamFile :: (KnownNat m, Enum addr)
            => SNat n             -- Size of the RAM
            -> FilePath           -- File with contents
            -> Signal Bool   -- wrE A
            -> Signal addr    -- Address A
            -> Signal (BitVector m) -- din A
          
            -> Signal Bool   -- wrE B
            -> Signal addr    --  Address B
            -> Signal (BitVector m) -- din B

            -> Signal (BitVector m, BitVector m) -- dout A, dout B
dpRamFile = dpRamFile' systemClock


{-# NOINLINE dpRamFile' #-}
dpRamFile' :: (KnownNat m, Enum addr)
            => SClock clk
            -> SNat n             -- Size of the RAM
            -> FilePath           -- File with contents
            -> Signal' clk Bool   -- wrE A
            -> Signal' clk addr    -- Address A
            -> Signal' clk (BitVector m) -- din A
          
            -> Signal' clk Bool   -- wrE B
            -> Signal' clk addr    --  Address B
            -> Signal' clk (BitVector m) -- din B

            -> Signal' clk (BitVector m, BitVector m) -- dout A, dout B
dpRamFile' clk sz file wreA addrA dinA wreB addrB dinB  = dpRamFile# clk sz file wreA (fromEnum <$> addrA) dinA wreB (fromEnum <$> addrB) dinB  


{-# NOINLINE  dpRamFile# #-}
dpRamFile# :: KnownNat m
            => SClock clk
            -> SNat n             -- Size of the RAM
            -> FilePath           -- File with contents
            -> Signal' clk Bool   -- wrE A
            -> Signal' clk Int    -- Address A
            -> Signal' clk (BitVector m) -- din A
          
            -> Signal' clk Bool   -- wrE B
            -> Signal' clk Int    --  Address B
            -> Signal' clk (BitVector m) -- din B

            -> Signal' clk (BitVector m, BitVector m) -- dout A, dout B

dpRamFile# clk sz file wreA addrA dinA wreB addrB dinB = register' clk undefined dout where
  szI  = fromInteger $ snatToInteger sz

  dout = runST $ do
      arr <- newListArray (0,szI-1) (initMem file)
      traverse (ramT arr) (bundle' clk (wreA, addrA, dinA, wreB, addrB, dinB))

  ramT :: STArray s Int e -> (Bool, Int, e, Bool, Int, e) -> ST s (e, e)
  ramT ram (eA, aA, dA, eB, aB, dB) = do
    dA' <- if (not eA) then readArray ram aA else return dA
    dB' <- if (not eB) then readArray ram aB else return dB
    when eA (writeArray ram aA dA)
    when eB (writeArray ram aB dB)
    return (dA', dB')


          
