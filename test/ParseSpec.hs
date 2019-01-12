module ParseSpec where
    import Test.Hspec
    import Test.QuickCheck
    import Control.Exception (evaluate)
    
    import Data.Word(Word8,Word16)
    import qualified Data.Vector.Unboxed as V
    import Parse
    import VRam
    import Opcode
    import Cpu

    getFakeVRam = VRam.init {
        ram = RAM (V.replicate 0x800 0xFF),
        ppu = PPU (V.empty),
        apu = APU (V.empty),
        openBus = OPB (V.empty),
        sram = SRM (V.empty),
        programRom = PRG (V.empty)
    }

    getFakeCpu = Cpu.empty

    fakeChunk = Chunk {code = 0xFF, upper = 0xFF, lower = 0xFF}
    
    spec :: Spec
    spec =  do
        describe "Parse.getWord16" $ do
            it "returns a Word16 giving two Word8" $ do
                getWord16 0xFF 0xFF `shouldBe` (0xFFFF :: Word16)

        describe "Parse.getWord8" $ do
            it "returns two Word8 from one Word16 (lower,upper)" $ do
                getWord8 0xFFAA `shouldBe` (0xAA :: Word8, 0xFF :: Word8)

        describe "Parse.getWord16FromChunk" $ do
            it "returns a Word16 from a Chunk" $ do
                getWord16FromChunk fakeChunk { upper = 0x11, lower = 0x22} `shouldBe` (0x1122) 
        
        describe "Parse.getWord16FromChunk" $ do
            it "throws an exception if Chunk is empty" $ do
                evaluate ( getWord16FromChunk Empty) `shouldThrow` anyErrorCall

        describe "Parse.getChunk" $ do   
            it "returns a Chunk giving a address" $ do
                getChunk 0x10 getFakeVRam `shouldBe` fakeChunk 

        describe "Parse.parseChunk" $ do   
            it "returns a Opcode giving a chunk" $ do
                parseChunk fakeChunk {code=0x01} `shouldBe` (ORA (InX (0xFF)))
                parseChunk fakeChunk {code=0x00} `shouldBe` (BRK None)
            it "throws an exception if Opcode is invalid " $ do
                    evaluate ( parseChunk fakeChunk {code=0x02}) `shouldThrow` anyException

        describe "Parse.readAddressMode" $ do
            it "returns Nothing if AddressMode is None" $ do
                readAddressMode (None) (getFakeCpu,getFakeVRam) `shouldBe` Nothing

            it "returns RegisterA if AddressMode is Acc" $ do
                readAddressMode (Acc) (getFakeCpu {registerA = 0xAA},getFakeVRam) `shouldBe` Just 0xAA
            
            it "returns the immediate value of Imm x" $ do
                readAddressMode (Imm 0xBB) (getFakeCpu,getFakeVRam) `shouldBe` Just 0xBB

            it "returns the data in the giving zero page address" $ do
                readAddressMode (Zpg 0x01) (getFakeCpu,getFakeVRam) `shouldBe` Just 0xFF

            it "returns the data in the giving zero page address + Register X" $ do
                let vram = VRam.write getFakeVRam 0x11 1
                readAddressMode (ZpX 0x10) (getFakeCpu {registerX = 0x01},vram) `shouldBe` Just 0x01

            it "returns the data in the giving zero page address + Register Y" $ do
                let vram = VRam.write getFakeVRam 0x12 0x22
                readAddressMode (ZpY 0x11) (getFakeCpu {registerY = 0x01},vram) `shouldBe` Just 0x22

            it "returns the data in the giving absolute address" $ do
                let vram = VRam.write getFakeVRam 0xB0 0x02
                readAddressMode (Abs 0x00B0) (getFakeCpu,vram) `shouldBe` Just 0x02 
                
            it "returns the data in the giving absolute address + Register X" $ do
                let vram = VRam.write getFakeVRam 0x07F2 0x03
                readAddressMode (AbX 0x07F0) (getFakeCpu {registerX = 0x02},vram) `shouldBe` Just 0x03

            it "returns the data in the giving absolute address + Register Y" $ do
                let vram = VRam.write getFakeVRam 0x07F4 0x05
                readAddressMode (AbY 0x07F0) (getFakeCpu {registerY = 0x04},vram) `shouldBe` Just 0x05

            it "returns the data in the giving indirect zero page address + Register X" $ do
                let vram = VRam.write getFakeVRam 0x01 0x05
                let vram1 = VRam.write vram 0x02 0x07
                let vram2 = VRam.write vram1 0x0705 0x0A
                readAddressMode (InX 0x00) (getFakeCpu {registerX = 0x01},vram2) `shouldBe` Just 0x0A

            it "returns the data in the giving indirect zero page address + Register Y" $ do
                let vram = VRam.write getFakeVRam 0x01 0x03
                let vram1 = VRam.write vram 0x02 0x07
                let vram2 = VRam.write vram1 0x0704 0x0B
                readAddressMode (InY 0x01) (getFakeCpu {registerY = 0x01},vram2) `shouldBe` Just 0x0B

            it "throws an error if Address Mode is invalid " $ do
                evaluate ( readAddressMode (Ind 0x0001) (getFakeCpu,getFakeVRam) ) `shouldThrow` anyErrorCall
