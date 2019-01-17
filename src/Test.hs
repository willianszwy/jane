{-# LANGUAGE OverloadedStrings #-}
module Test where
    import Cpu as C
    import VRam as VR
    import Parse as P
    import Opcode
    import Rom as R
    import qualified Data.ByteString as B

    main :: IO ()
    main = do
        rom <- R.loadRom "../registers.nes"
        header <- R.parseRom rom
        let vram = loadProgramRom VR.init (B.take 0x8000 $ B.drop 16 rom)
        let powerOn = P.execOpcode (JMP (Ind 0xFFFC)) (C.empty,vram)
        runCPU powerOn
        return ()
    

    runCPU :: (Cpu,VRam) -> IO ()
    runCPU (cpu,vram) = do
            let op = next (cpu,vram)
            print op
            let (cpu',vram') = P.execOpcode op (cpu,vram)
            -- print cpu'
            runCPU $ (cpu',vram') 
        where             
            next (cpu1,vram1) = parseChunk $ getChunk (programCounter cpu1) vram1

   
    
    
    
    
  
  