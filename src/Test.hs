{-# LANGUAGE OverloadedStrings #-}
module Test where
import           Cpu                           as C
import           VRam                          as VR
import           Parse                         as P
import           Opcode
import           Rom                           as R
import qualified Data.ByteString               as B
import           Control.Monad.State
import           Control.Monad.IO.Class         ( liftIO )

main :: IO ()
main = do
    rom    <- R.loadRom "../registers.nes"
    header <- R.parseRom rom
    let vram    = loadProgramRom VR.init (B.take 0x8000 $ B.drop 16 rom)
    let powerOn = P.execOpcode (JMP (Ind 0xFFFC)) (C.empty, vram)
    evalStateT runCPU powerOn



runCPU :: StateT (Cpu, VRam) IO ()
runCPU = do
    (cpu, vram) <- get
    let op = fetch (cpu, vram)
    put $ P.execOpcode op (cpu, vram)
    liftIO $ print op
    runCPU
  where
    fetch (cpu1, vram1) = parseChunk $ getChunk (programCounter cpu1) vram1








