module Cpu where 
    
    import Data.Word
    import Flags as F

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

    newtype PS = PS Flags
        deriving (Show)

    data Cpu = Cpu A X Y PC SP PS
           deriving (Show)

    showFlags :: Cpu -> IO ()
    showFlags (Cpu _ _ _ _ _ (PS f)) = putStrLn $ F.printFlags f

    setFlags :: Cpu -> Flags -> Cpu
    setFlags (Cpu (A a) (X x) (Y y) (PC pc) (SP sp) _ ) flags = Cpu (A a) (X x) (Y y) (PC pc) (SP sp) (PS flags)