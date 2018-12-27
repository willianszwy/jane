module Cpu where 
    
    import Data.Word

    newtype A = A Word8 
        deriving (Show)

    newtype X = X Word8 
        deriving (Show)

    newtype Y = Y Word8 
        deriving (Show)

    newtype PC = PC Word16 
        deriving (Show)

    newtype SP = SP Word8  
        deriving (Show)

    newtype PS = PS Word8 
        deriving (Show)

    data Cpu = Cpu A X Y PC SP PS
           deriving (Show)

    
    printFlags :: PS -> IO ()
    printFlags (PS x) = print x