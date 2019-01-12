{-# LANGUAGE OverloadedStrings #-}
module Test where
    import           Data.Word(Word8, Word16)
    import Rom
    import VRam
    import Cpu
    import qualified Data.ByteString as B

    main :: IO ()
    main = do        
        rom <- loadRom "../QuattroAdventure.nes"
        hd <- parseRom rom
        print hd
        return ()

  
    -- vram = VRam.init
    -- cpu = Cpu.empty
    -- opcode =  VRam.read vram (programCounter cpu)

    -- endereco
    -- tipo 
    -- parametros
    -- executar 


    -- IMPORTANTE 0xFFFC .|. ( shift 0xFFFD 8)  

    -- 58015