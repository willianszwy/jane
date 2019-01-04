{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Cpu where 
    
    import Data.Word
    import Flags as F

    data Cpu = Cpu { accumulator :: Word8 ,
                     indexX :: Word8 ,
                     indexY :: Word8 ,
                     programCounter :: Word16 ,
                     stackPointer :: Word8,
                     processorStatus :: Flags
                     } deriving (Show)


    empty :: Cpu
    empty = Cpu { accumulator = 0,
                  indexX = 0,
                  indexY = 0,
                  programCounter = 0,
                  stackPointer = 0,
                  processorStatus = F.empty
                }


    showFlags :: Cpu -> String
    showFlags Cpu {processorStatus} = F.printFlags processorStatus

    setFlags :: Cpu -> Flags -> Cpu
    setFlags cpu flags = cpu {processorStatus=flags}