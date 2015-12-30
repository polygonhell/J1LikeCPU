{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}


module Main where

import CLaSH.Prelude
import CLaSH.Sized.Signed

import Language.Haskell.TH
import CLaSH.Promoted.Nat
import Debug.Trace
import Text.Printf
import qualified Data.List as L

import Types
import Stack
import DPBRam

$(decLiteralD 8192)
$(decLiteralD 16384)
$(decLiteralD 20480)
$(decLiteralD 65536)


{-# ANN topEntity
  (TopEntity
    { t_name = "main"
    , t_inputs = []
    , t_outputs = ["SS_SEGS"]
    , t_extraIn = [ ("SYS_CLK", 1)
                  ]
    , t_extraOut = []
    , t_clocks   = [ (clockWizard "clkwiz50"
                             "SYS_CLK(0)"
                             "'0'") 
                   ]
}) #-}  


topEntity :: Signal (BitVector 4)
topEntity = o where 
  res = system
  o = (resize . dOut) <$> res



combineBits :: BitVector 32 -> BitVector 8
combineBits a = b1 `xor` b2 where
  (w1, w2) = split a :: (BitVector 16, BitVector 16)
  (b1a, b1b) = split w1 :: (BitVector 8, BitVector 8)
  (b2a, b2b) = split w2 :: (BitVector 8, BitVector 8)
  b1 = b1a `xor` b1b
  b2 = b2a `xor` b2b


ram addrA addrB weB dataB = dpRamFile d16384 "rob.bin" (signal False) addrA (signal 0) weB addrB dataB

-- ram32K :: Signal Addr -> Signal Bit -> Signal Byte -> Signal Byte
-- -- ram64K addr wrEn dataIn = blockRamPow2 testRAMContents addr addr wrEn dataIn
-- ram32K aAddr wrEn dataA bAddr dataB = unpack <$> blockRamFilePow2 "rob.bin" (unpack <$> addr) (unpack <$> addr) wrEn (pack <$> dataIn)


system :: Signal CpuOut
system = out where
  out = evalM $ (CpuIn <$> iin <*> din)
  addr = iOutAddr <$> out :: Signal AddrSize
  (din, _) = unbundle $ ram addr (dOutAddr <$> out) ((==) 1 <$> (dOutWE <$> out)) (dOut <$> out)
  iin = resize <$> din :: Signal InstructionWidth  -- TODO this is incorrect


runSystem x = putStr $ unlines $ L.map (show) $L.drop 1 (sampleN x system)

main = runSystem 10



-- DataIn 
type InstructionWidth = BitVector 16
data CpuIn = CpuIn 
  { instruction :: InstructionWidth,
    dataIn :: WordSize
  } deriving (Show)

data CpuOut = CpuOut
  { -- Data Out controll
    dOut :: WordSize,           
    dOutAddr :: AddrSize,
    dOutWE :: Bit,

    -- Instruction Fetch Address
    iOutAddr :: AddrSize
  } deriving Show

data CpuState = CpuState
  { pc :: AddrSize,
    dstT :: WordSize,
    stDepth :: BitVector 5,
    -- Data and return Stacks
    dst :: Stack 32 32,
    rst :: Stack 32 15
  }

initialState = CpuState 0 0 0 (Stack 0 (replicate d32 (0 :: WordSize))) (Stack 0 (replicate d32 (0 :: AddrSize)))
evalM = eval `mealy` initialState

eval :: CpuState -> CpuIn -> (CpuState, CpuOut) 
eval CpuState{..} CpuIn{..} = (st', out) where
  st' = CpuState pc' dstT' stDepth' dst' rst'
  out = CpuOut dOut (resize dstT) dstWe pc' 

  pc' = pc + 1 -- TODO needs to accomodate jumps etc

  (dst', dstN) = stack dst (dstWe, dstDelta, dstT)
  (rst', rstR) = stack rst (0, 0, 0)

  dOut = dstN
  dstWe = if isALU && write then 1 else 0 :: Bit
  dstDelta = 0
  stDepth' = 0

  instMode = slice d15 d13 instruction
  dstT' = case instMode of
     0b000 -> dstT                                  -- Jump
     0b001 -> dstN                                  -- Conditional Jump
     0b010 -> dstT                                  -- Call
     0b011 -> aluRes                                -- AluOp needs to be decoded
     _ -> resize $ slice d14 d0 instruction         -- Immediate Load

  aluSelect = slice d12 d8 instruction
  shiftAmt = fromIntegral (dstT .&. 0x1f) :: Int
  aluRes = case aluSelect of
    0b0000 -> dstT   -- T
    0b0001 -> dstN   -- N
    0b0010 -> dstT + dstN
    0b0011 -> dstT .&. dstN
    0b0100 -> dstT .|. dstN
    0b0101 -> dstT `xor` dstN
    0b0110 -> complement dstT
    0b0111 -> if dstN == dstT then -1 else 0
    0b1000 -> if sDstN < sDstT then -1 else 0 where -- Signed comparison
      sDstN = unpack dstN :: Signed 32
      sDstT = unpack dstT :: Signed 32
    0b1001 -> dstN `shiftR` shiftAmt
    0b1010 -> dstN `shiftL` shiftAmt
    0b1011 -> resize rstR    -- R
    0b1100 -> dataIn  -- Read from memory
    0b1101 -> 0       -- io_din??
    0b1110 -> 0       -- depth
    0b1111 -> if dstN < dstT then -1 else 0         -- Unsigned comparison


  functionBits = slice d6 d4 instruction
  t_N = functionBits == 1
  t_R = functionBits == 2
  write = functionBits == 3
  iow = functionBits == 4
  ior = functionBits == 5
  isALU = instMode == 0b011





  








