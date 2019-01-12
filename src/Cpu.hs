{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Cpu where 
    
    import Data.Word
    import Flags as F

    data Cpu = Cpu { 
                     registerA :: Word8 ,
                     registerX :: Word8 ,
                     registerY :: Word8 ,
                     programCounter :: Word16 ,
                     stackPointer :: Word8,
                     processorStatus :: Flags
                   } deriving (Show)

    empty :: Cpu
    empty = Cpu { 
                  registerA = 0,
                  registerX = 0,
                  registerY = 0,
                  programCounter = 0x34,
                  stackPointer = 0xFD,
                  processorStatus = F.empty
                }

    setFlags :: Cpu -> Flags -> Cpu
    setFlags cpu flags = cpu {processorStatus=flags}