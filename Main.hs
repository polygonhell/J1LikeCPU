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
    , t_clocks   = [ clockWizard "clkwiz50"
                             "SYS_CLK(0)"
                             "'0'" 
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
  out = evalM (CpuIn <$> iin <*> din)

  addr = iOutAddr <$> out :: Signal AddrSize  
  lowBit = lsb <$> addr :: Signal (BitVector 1)

  (iWord, din) = unbundle $ ram ((\x -> x `shiftR` 1) <$> addr) (dOutAddr <$> out) ((==) 1 <$> (dOutWE <$> out)) (dOut <$> out) :: (Signal WordSize, Signal WordSize)
  iin = fn <$> iWord <*> lowBit :: Signal InstructionWidth  -- TODO this is incorrect
  fn dd lb = inst where
    shift = if lb == 0 then 16 else 0
    inst = resize $ dd `shiftR` shift :: InstructionWidth


runSystem x = putStr $ unlines $ L.map show $ L.drop 1 (sampleN x system)

stackTest = stOut where
  st = (Stack 0 (replicate d32 (0 :: WordSize)))
  (st', _) =  stack st (1, 1, 1234)
  (stOut, _) =  stack st' (1, 1, 12345)

main = runSystem 5



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
    iOutAddr :: AddrSize,

    -- Debugging state 
    dbgReboot :: Bool,
    dbgInstruction :: InstructionWidth,
    dbgData :: WordSize,
    dbgTopStack :: WordSize,
    dbgDSWe :: Bit,
    dbgDSDelta :: BitVector 2,
    dbgDataStck :: Stack 32 32
  } deriving Show

data CpuState = CpuState
  { reboot :: Bool,
    pc :: AddrSize,
    dstT :: WordSize,
    stDepth :: BitVector 5,
    -- Data and return Stacks
    dst :: Stack 32 32,
    rst :: Stack 32 15
  } deriving Show

initialState = CpuState True 0 0 0 (Stack 0 (replicate d32 (0 :: WordSize))) (Stack 0 (replicate d32 (0 :: AddrSize)))
evalM = eval `mealy` initialState

eval :: CpuState -> CpuIn -> (CpuState, CpuOut) 
eval state_in@CpuState{..} CpuIn{..} = (st', out) where

  st' = CpuState reboot' pc' dstT' stDepth' dst' rst'
  out = CpuOut dOut (resize dstT) 0 pc' reboot instruction dataIn dstT' dstWe dstDelta dst'
  reboot' = False

  pcPlusOne = pc+1
  pc' = if reboot then 0 else pcPlusOne -- TODO needs to accomodate jumps etc

  (dst', dstN) = if reboot then (dst, 0) else stack dst (dstWe, dstDelta, dstT)
  (rst', rstR) = stack rst (0, 0, 0)

  dOut = dstN
  stDepth' = 0

  instMode = slice d15 d13 instruction
  dstT' = if reboot then 0 else
    case instMode of
     0b000 -> dstT                                  -- Jump
     0b001 -> dstN                                  -- Conditional Jump
     0b010 -> dstT                                  -- Call
     0b011 -> aluRes                                -- AluOp needs to be decoded
     _ -> resize $ slice d14 d0 instruction         -- Immediate Load


  -- TODO Bit 12

  aluSelect = slice d11 d8 instruction :: BitVector 4
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


  (dstWe, dstDelta) = case instMode of
      0b000 -> (1, 0)
      0b001 -> (1, 0b11)
      0b010 -> (1, 0)
      0b011 -> (if t_N then 1 else 0, slice d1 d0 instruction)
      _ -> (1, 0b01)



  








