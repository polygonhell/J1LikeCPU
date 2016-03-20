{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module UART where

import CLaSH.Prelude
import qualified Data.List as L

data TXState = TXState { txDataReg :: BitVector 8
                       , txEmpty :: Bool
                       , txCount :: BitVector 4
                       , txOut :: Bit
                       , clockDividerCount :: BitVector 13
                       } deriving (Show)


divider :: BitVector 13
divider = 5207      -- 50E6/9600 -1

uartInitialState :: TXState
uartInitialState = TXState 0 True 0 0 0

txUart :: Signal (BitVector 8, Bool, Bool) -> Signal Bit
txUart =  txRun `mealy` uartInitialState

testUART :: IO ()
testUART = putStr $ "divisor = " L.++ show divider

txRun :: TXState -> (BitVector 8, Bool, Bool) -> (TXState, Bit) 
txRun st@TXState{..} (txData, txLoadData, txEnable) = (st', out) where
    (st', out) = case clockDividerCount of
        _ | clockDividerCount == divider -> st'' where
            txDataReg'  = if txLoadData then txData else txDataReg
            txEmpty' = case txLoadData of 
                True -> False
                _ | txCount == 9 -> True
                _ -> txEmpty
            txOut' = if not txEnable then 0 else case txCount of
                0 -> 0
                9 -> 1
                _ -> txDataReg ! (txCount -1)
            txCount' = case txCount of
                9 -> 0
                _ -> txCount + 1

            st'' = (st{clockDividerCount = 0, txDataReg = txDataReg', txEmpty = txEmpty', txOut = txOut', txCount = txCount'}, txOut')
        _ -> st'' where
            st'' = (st{clockDividerCount = clockDividerCount+1}, txOut)

