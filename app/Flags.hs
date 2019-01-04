module Flags where
import           Data.Word
import           Data.Bits

type Flags = Word8

empty :: Flags
empty = setBit zeroBits 5


data Flag = Negative | Overflow | Check | Break | Decimal | Interrupt | Zero | Carry deriving (Eq, Show)


setFlag :: Flags -> Flag -> Flags
setFlag flags Carry     = setBit flags 0
setFlag flags Zero      = setBit flags 1
setFlag flags Interrupt = setBit flags 2
setFlag flags Decimal   = setBit flags 3
setFlag flags Break     = setBit flags 4
setFlag flags Overflow  = setBit flags 6
setFlag flags Negative  = setBit flags 7


clearFlag :: Flags -> Flag -> Flags
clearFlag flags Carry     = clearBit flags 0
clearFlag flags Zero      = clearBit flags 1
clearFlag flags Interrupt = clearBit flags 2
clearFlag flags Decimal   = clearBit flags 3
clearFlag flags Break     = clearBit flags 4
clearFlag flags Overflow  = clearBit flags 6
clearFlag flags Negative  = clearBit flags 7


testFlag :: Flags -> Flag -> Bool
testFlag flags Carry     = testBit flags 0
testFlag flags Zero      = testBit flags 1
testFlag flags Interrupt = testBit flags 2
testFlag flags Decimal   = testBit flags 3
testFlag flags Break     = testBit flags 4
testFlag flags Check     = testBit flags 5
testFlag flags Overflow  = testBit flags 6
testFlag flags Negative  = testBit flags 7


printFlags :: Flags -> String
printFlags flags =
    let c = " | C: " ++ testFlag' Carry
        z = " | Z: " ++ testFlag' Zero
        i = " | I: " ++ testFlag' Interrupt
        d = " | D: " ++ testFlag' Decimal
        b = " | B: " ++ testFlag' Break
        x = " | -: " ++ testFlag' Check
        v = " | V: " ++ testFlag' Overflow
        n = "N: " ++ testFlag' Negative
    in  concat $ reverse [c, z, i, d, b, x, v, n]
    where testFlag' = show . fromEnum . testFlag flags
