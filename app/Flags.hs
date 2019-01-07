{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Flags where
import           Data.Word
import           Data.Bits

data Flags = Flags { 
                     negative :: Bool,
                     overflow :: Bool,
                     break :: Bool,
                     decimal :: Bool,
                     interrupt :: Bool,
                     zero :: Bool,
                     carry :: Bool
                   } deriving (Show)

empty :: Flags
empty = Flags { 
    negative = False,
    overflow = False,
    break = False,
    decimal = False,
    interrupt = False,
    zero = False,
    carry = False
  } 

compact :: Flags -> Word8
compact flags = 
    let f = setBit 0 5 in
    isNegative . isOverflow . isBreak . isDecimal . isInterrupt . isZero . isCarry $ f
    where 
        isNegative w  = if negative flags then setBit w 7 else clearBit w 7
        isOverflow w  = if overflow flags then setBit w 6 else clearBit w 6
        isBreak w     = if Flags.break flags then setBit w 4 else clearBit w 4
        isDecimal w   = if decimal flags then setBit w 3 else clearBit w 3
        isInterrupt w = if interrupt flags then setBit w 2 else clearBit w 2
        isZero w      = if zero flags then setBit w 1 else clearBit w 1
        isCarry w     = if carry flags then setBit w 0 else clearBit w 0

extract :: Word8 -> Flags
extract word = 
    Flags {
        negative = testBit word 7,
        overflow = testBit word 6,
        break = testBit word 4,
        decimal = testBit word 3,
        interrupt = testBit word 2,
        zero = testBit word 1,
        carry = testBit word 0
    }