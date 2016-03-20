{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}


module Main where

import CLaSH.Prelude
import Text.Printf
import qualified Data.List as L
import Prelude hiding (replicate)
-- import Debug.Trace

import UART

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
    , t_outputs = ["LED", "TX"]
    , t_extraIn = [ ("SYS_CLK", 1)
                  ]
    , t_extraOut = []
    , t_clocks   = [ clockWizard "clkwiz50"
                             "SYS_CLK(0)"
                             "'0'" 
                   ]
}) #-}  


topEntity :: Signal (BitVector 4, BitVector 1)
topEntity = bundle (res, tx) where 
  res = signal 10 :: Signal (BitVector 4)
  tx = uart

-- topEntity :: Signal (BitVector 4, BitVector 1)
-- topEntity = bundle (o, tx) where 
--   res = system
--   o = (resize . dOut) <$> res
--   tx = uart


uart :: Signal Bit
uart = out where 
  (out, rdy) = unbundle $ txUart $ bundle (dataIn, load, signal True :: Signal Bool)
  dataIn = signal 99 :: Signal (BitVector 8)
  load = register True nextLoad
  nextLoad = rdy


combineBits :: BitVector 32 -> BitVector 8
combineBits a = b1 `xor` b2 where
  (w1, w2) = split a :: (BitVector 16, BitVector 16)
  (b1a, b1b) = split w1 :: (BitVector 8, BitVector 8)
  (b2a, b2b) = split w2 :: (BitVector 8, BitVector 8)
  b1 = b1a `xor` b1b
  b2 = b2a `xor` b2b

ram :: forall addr (m :: Nat).
       (Enum addr, KnownNat m) =>
       Signal addr
       -> Signal addr
       -> Signal Bool
       -> Signal (BitVector m)
       -> Signal (BitVector m, BitVector m)
-- -- ram addrA addrB weB dataB = dpRamFile d128 "rob.bin" (signal False) addrA (signal 0) weB addrB dataB
ram addrA addrB weB = dpRamFile d128 "rob.bin" (signal False) addrA (signal 0) weB addrB


  -- bram addrA _ _ _= bundle (iOut, dOut) :: Signal (WordSize, WordSize) where
  -- iOut = blockRamFile d128 "rob.bin" (signal (0::AddrSize)) addrA (signal False) (signal (0::WordSize)) :: Signal WordSize
  -- dOut = signal (0::WordSize)

-- ram32K :: Signal Addr -> Signal Bit -> Signal Byte -> Signal Byte
-- -- ram64K addr wrEn dataIn = blockRamPow2 testRAMContents addr addr wrEn dataIn
-- ram32K aAddr wrEn dataA bAddr dataB = unpack <$> blockRamFilePow2 "rob.bin" (unpack <$> addr) (unpack <$> addr) wrEn (pack <$> dataIn)


system :: Signal CpuOut
system = out where
  out = evalM (CpuIn <$> iin <*> din)

  addr = iOutAddr <$> out :: Signal AddrSize  
  -- Need a FlipFlop to delay the lowBit so it's correctly applied to the Instruction after the RAM fetch
  lowBit = register 0 $ lsb <$> addr :: Signal (BitVector 1)

  (iWord, din) = unbundle $ ram (wordAddr <$> addr) (dOutAddr <$> out) ((==) 1 <$> (dOutWE <$> out)) (dOut <$> out) :: (Signal WordSize, Signal WordSize)
  wordAddr x = x `shiftR` 1
  iin = fn <$> iWord <*> lowBit
  fn dd lb = instr where
    bshift = if lb == 0 then 16 else 0
    instr = resize $ dd `shiftR` bshift 

runSystem :: Int -> IO ()
runSystem x = putStr $ unlines $ L.map show $ sampleN x system

main :: IO()
main = runSystem 20

data InstructionMode = ImALU | ImJmp | ImJmpNE | ImCall deriving (Show)
data AluOp = AluT | AluN | AluX | AluNotT | AluMinusT | AluTMinus1
           | AluAdd deriving (Show)
data NSelect = NsN | NsT | NsX | NsTtoR deriving (Show)
data XSelect = XsX | XsT | XsN | XsWriteT deriving (Show)


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
    dbgDataStck :: Stack 31 32,
    dbgRetStack :: Stack 31 32
  }

instance Show CpuOut where
  show a = str where 
    CpuOut{..} = a
    str = printf "%08x " (toInteger dOut) L.++ 
          printf "%04x " (toInteger dOutAddr) L.++ 
          printf "%01x " (toInteger dOutWE) L.++ 
          printf "%04x " (toInteger iOutAddr) L.++ 

          printf "%01x " (if dbgReboot then 1 else 0 :: Integer) L.++ 
          printf "%04x " (toInteger dbgInstruction) L.++ 
          printf "%08x " (toInteger dbgData) L.++ 
          printf "%01x " (toInteger dbgDSWe) L.++ 
          printf "%01x " (toInteger dbgDSDelta) L.++ 
          printf "%08x " (toInteger dbgTopStack) L.++ 
          show dbgDataStck L.++
          " " L.++ show dbgRetStack

data CpuState = CpuState
  { reboot :: Bool,
    pc :: AddrSize,
    t :: WordSize,
    depth :: BitVector 5,
    -- Data and return Stacks
    dst :: Stack 31 32,
    rst :: Stack 31 32
  } deriving Show

initialState :: CpuState
initialState = CpuState True 0 0 0 (Stack 0 0 (replicate d31 (0 :: WordSize))) (Stack 0 0 (replicate d31 (0 :: WordSize)))

evalM :: Signal CpuIn -> Signal CpuOut
evalM = eval `mealy` initialState


eval :: CpuState -> CpuIn -> (CpuState, CpuOut) 
eval CpuState{..} CpuIn{..} = (st', out) where


  (st', out) = 
    if reboot then 
      (CpuState False pc t depth dst rst, CpuOut t 0 0 pc reboot 0 0 t 0 0 dst rst) 
    else
      (CpuState False pc' t' depth' dst' rst', CpuOut t' 0 0 pc' reboot instruction dataIn t' dstWe dstDelta dst' rst')

  Stack n x _ = dst
  Stack rh _ _ = rst

  pc1 = pc + 1
  (pc', rst') = if isImm then
          (pc1, rst)
        else
          case iMode of
            ImJmp -> (branchTarget, rst)
            ImJmpNE | t /= 0 -> (branchTarget, rst)  
            ImCall -> (branchTarget, rst'') where
              rst'' = stack rst (resize pc1, rh, 1, 1)
            ImALU | isRet -> (resize rh, rst'') where
              rst'' = stack rst (0, 0, 0, -1)
            _ -> (pc1, rst)

  dst' = stack dst (n', x', dstWe, dstDelta) 
  depth' = 0

  isRet = instruction ! (2 :: Integer) == 1
  isImm = instruction ! (15 :: Integer) == 1
  iMode = case slice d14 d13 instruction :: BitVector 2 of
    0x00 -> ImCall
    0x01 -> ImJmp
    0x02 -> ImJmpNE
    _ -> ImALU

  branchTarget = resize $ slice d12 d0 instruction :: AddrSize

  aluOp = case slice d11 d7 instruction of
    0x00 -> AluT
    0x01 -> AluN
    0x02 -> AluX
    0x03 -> AluNotT
    0x04 -> AluMinusT
    0x05 -> AluTMinus1
    0x06 -> AluAdd
    _ -> AluT

  nSelect = case slice d6 d5 instruction of
    0x01 -> NsT
    0x02 -> NsX
    0x03 -> NsTtoR
    _ -> NsN

  xSelect = case slice d4 d3 instruction of
    0x01 -> XsT
    0x02 -> XsN
    0x03 -> XsWriteT
    _ -> XsX

  dstackWrite = complement $ dstackOffset ! (1 :: Integer) -- 0 or positive offset causes write
  dstackOffset = slice d1 d0 instruction

  complementT = complement t
  -- Share the adder for these
  adderOut = a + b where
    (a,b) = case aluOp of
      AluTMinus1 -> (t, -1)
      AluMinusT -> (complementT, 1)
      _ -> (t, n) 

  immValue = resize $ instruction .&. 0x7fff :: WordSize

  (t', n', x', dstWe, dstDelta) = 
    if isImm then (immValue, t, n, 1, 1)  else aluVal

  -- aluVal :: (WordSize, WordSize, WordSize, Bit, BitVector 2)
  aluVal = case iMode of
    ImALU -> (res, nOut, xOut, dstackWrite, dstackOffset) where
      res = case aluOp of
        AluT -> t
        AluN -> n
        AluX -> x
        AluNotT -> complement t
        AluMinusT -> adderOut
        AluTMinus1 -> adderOut
        AluAdd -> adderOut
      nOut = case nSelect of
        NsT -> t
        NsX -> x
        _ -> n
      xOut = case xSelect of
        XsT -> t
        XsN -> n
        _ -> x
    ImJmpNE -> (n, 0, 0, 0, -1)
    _ -> (t, 0, 0, 0, 0)















