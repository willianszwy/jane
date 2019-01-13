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
    import Flags

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
        describe "Parse.setNegative" $ do
            it "returns True when given a negative number input" $ do
                setNegative 0xFF `shouldBe` True
        
            it "returns False when given a positive number input" $ do 
                setNegative 0x7F `shouldBe` False
        
        describe "Parse.setZero" $ do 
            it "returns True when given a zero input" $
                setZero 0x00 `shouldBe` True
            
            it "return False when given a not zero input" $ 
                setZero 0x01 `shouldBe` False
        
        describe "Parse.setOverflow" $ do
            it "returns False when given a input overflow signed operations" $ 
                setOverflow 0x01 0x01 (0x01 + 0x01) `shouldBe` False
        
            it "returns False when given a input overflow signed operations" $ 
                setOverflow 0x01 0xFF (0x01 + 0xFF) `shouldBe` False
            
            it "returns True when given a input overflow signed operations" $ 
                setOverflow 0x7F 0x01 (0x7F + 0x01) `shouldBe` True
            
            it "returns True when given a input overflow flag on signed operations" $ 
                setOverflow 0x80 0xFF (0x80 + 0xFF) `shouldBe` True
    
        describe "Parse.setCarry" $ do
            it "returns False when given a input carry flag on unsigned operations" $ 
                setCarry (0x01 + 0x01) `shouldBe` False
    
            it "returns True when given a input carry flag on unsigned operations" $ 
                setCarry (0xFF + 0xFF) `shouldBe` True

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

        describe "Parse.adc" $ do
            it "Add Memory to Accumulator with Carry" $ do
                let op = ADC (Imm 0xFF)
                let flags = processorStatus getFakeCpu
                adc op (getFakeCpu { registerA = 0xFF},getFakeVRam) `shouldBe` 
                 (getFakeCpu{ registerA = 0xFE,processorStatus= flags {carry=True,negative=True}},getFakeVRam)

        describe "Parse.clc" $ do
            it "cleans the carry flag" $ do
                let flags = processorStatus getFakeCpu
                clc getFakeCpu {processorStatus = flags { carry=False}}
                  `shouldBe` getFakeCpu {processorStatus = flags { carry=False}}

        describe "Parse.sec" $ do
            it "set the carry flag" $ do
                let flags = processorStatus getFakeCpu
                sec getFakeCpu `shouldBe` getFakeCpu {processorStatus = flags { carry=True}}

        describe "Parse.cli" $ do
            it "cleans the interrupt flag" $ do
                let flags = processorStatus getFakeCpu
                cli getFakeCpu {processorStatus = flags { interrupt=False}}
                    `shouldBe` getFakeCpu {processorStatus = flags { interrupt=False}}
        
        describe "Parse.sei" $ do
            it "set the interrupt flag" $ do
                let flags = processorStatus getFakeCpu
                sei getFakeCpu `shouldBe` getFakeCpu {processorStatus = flags { interrupt=True}}

        describe "Parse.clv" $ do
            it "cleans the overflow flag" $ do
                let flags = processorStatus getFakeCpu
                clv getFakeCpu {processorStatus = flags { overflow=False}}
                    `shouldBe` getFakeCpu {processorStatus = flags { overflow=False}}

        describe "Parse.cld" $ do
            it "cleans the decimal flag" $ do
                let flags = processorStatus getFakeCpu
                cld getFakeCpu {processorStatus = flags { decimal=False}}
                    `shouldBe` getFakeCpu {processorStatus = flags { decimal=False}}
                    
        describe "Parse.sed" $ do
            it "set the decimal flag" $ do
                let flags = processorStatus getFakeCpu
                sed getFakeCpu `shouldBe` getFakeCpu {processorStatus = flags { decimal=True}}

        describe "Parse.sta" $ do
            it "Store Accumulator in Memory" $ do
                let vram = VRam.write getFakeVRam 0xAA 0xA1
                sta (STA (Zpg 0xAA)) (getFakeCpu {registerA = 0xA1},getFakeVRam) 
                    `shouldBe` (getFakeCpu {registerA = 0xA1},vram)

        describe "Parse.stx" $ do
            it "Store Index X in Memory" $ do
                    let vram = VRam.write getFakeVRam 0xBB 0xA2
                    stx (STX (Zpg 0xBB)) (getFakeCpu {registerX = 0xA2},getFakeVRam) 
                        `shouldBe` (getFakeCpu {registerX = 0xA2},vram)

        describe "Parse.sty" $ do
            it "Store Index Y in Memory" $ do
                    let vram = VRam.write getFakeVRam 0xCC 0xA3
                    sty (STY (Zpg 0xCC)) (getFakeCpu {registerY = 0xA3},getFakeVRam) 
                        `shouldBe` (getFakeCpu {registerY = 0xA3},vram)

        -- Stack

        describe "Parse.txs" $ do
            it "Transfer X to Stack Pointer" $ do
                txs (getFakeCpu {registerX = 0xBB, stackPointer = 0xFF})
                    `shouldBe` getFakeCpu {registerX = 0xBB, stackPointer = 0xBB}
        
        describe "Parse.tsx" $ do
            it "Transfer Stack Pointer to X" $ do
                tsx (getFakeCpu {registerX = 0xBB, stackPointer = 0xFF})
                    `shouldBe` getFakeCpu {registerX = 0xFF, stackPointer = 0xFF}
                        
        describe "Parse.pha" $ do
            it "Push Accumulator to Stack" $ do
                let vram = VRam.write getFakeVRam 0x1FF 0xCC
                pha (getFakeCpu{registerA = 0xCC, stackPointer = 0xFF},getFakeVRam) 
                    `shouldBe` (getFakeCpu{registerA = 0xCC, stackPointer = 0xFE},vram)

            it "Push Accumulator to Stack should overflow" $ do
                let vram = VRam.write getFakeVRam 0x100 0xDD
                pha (getFakeCpu{registerA = 0xDD, stackPointer = 0x00},getFakeVRam) 
                    `shouldBe` (getFakeCpu{registerA = 0xDD, stackPointer = 0xFF},vram)

        describe "Parse.pla" $ do
            it "Pull Stack to Acummulator" $ do
                let vram = VRam.write getFakeVRam 0x1FE 0xA1
                let flags = (processorStatus getFakeCpu){negative = setNegative 0xA1, zero = setZero 0xA1}
                pla (getFakeCpu{registerA = 0x00, stackPointer = 0xFD},vram) 
                    `shouldBe` (getFakeCpu{registerA = 0xA1, stackPointer = 0xFE,processorStatus=flags},vram)

            it "Push Accumulator to Stack should overflow" $ do
                let vram = VRam.write getFakeVRam 0x100 0xD1
                let flags = (processorStatus getFakeCpu){negative = setNegative 0xD1, zero= setZero 0xD1}
                pla (getFakeCpu{registerA = 0x00, stackPointer = 0xFF},vram) 
                    `shouldBe` (getFakeCpu{registerA = 0xD1, stackPointer = 0x00, processorStatus=flags},vram)
       
        describe "Parse.php" $ do
            it "Push Processor Status to Stack" $ do
                let flags = (processorStatus getFakeCpu){carry=True,negative=True}
                let status = compact flags
                let vram = VRam.write getFakeVRam 0x1FD status
                php (getFakeCpu{processorStatus=flags},getFakeVRam)
                    `shouldBe` (getFakeCpu{processorStatus=flags,stackPointer = 0xFC},vram)
                        
        describe "Parse.plp" $ do 
            it "Pull Stack to Processor Status" $ do
                let flags = (processorStatus getFakeCpu){carry=True,interrupt=True}
                let status = compact flags 
                let vram = VRam.write getFakeVRam 0x1FE status
                plp (getFakeCpu{stackPointer=0xFD},vram) 
                    `shouldBe` (getFakeCpu{stackPointer=0xFE,processorStatus=flags},vram)
        

        describe "Parse.tax" $ do
            it "Transfer Accumulator to X" $ do
                let flags = (processorStatus getFakeCpu)
                tax getFakeCpu{registerA=0xBA,registerX=0xBC} 
                    `shouldBe` getFakeCpu{registerA=0xBA,registerX=0xBA,
                        processorStatus=flags{negative=setNegative 0xBA,zero=setZero 0xBA}}

        
        describe "Parse.txa" $ do
            it "Transfer Index X to A" $ do
                let flags = (processorStatus getFakeCpu)
                txa getFakeCpu{registerA=0xC1,registerX=0xC5} 
                   `shouldBe` getFakeCpu{registerA=0xC5,registerX=0xC5,
                        processorStatus=flags{negative=setNegative 0xC5,zero=setZero 0xC5}}

        describe "Parse.dex" $ do
            it "Decrement Index X" $ do
                let flags = (processorStatus getFakeCpu)
                let newX = (registerX getFakeCpu) - 1 :: Word8
                dex getFakeCpu{registerX=newX + 1} 
                  `shouldBe` getFakeCpu{registerX=newX,
                        processorStatus=flags{negative=setNegative newX,zero=setZero newX}}
    
        describe "Parse.inx" $ do
            it "Increment Index X" $ do
                let flags = (processorStatus getFakeCpu)
                let newX = (registerX getFakeCpu) + 1 :: Word8
                inx getFakeCpu{registerX=newX - 1} 
                    `shouldBe` getFakeCpu{registerX=newX,
                            processorStatus=flags{negative=setNegative newX,zero=setZero newX}}


        describe "Parse.tay" $ do
            it "Transfer Accumulator to Y" $ do
                let flags = (processorStatus getFakeCpu)
                tay getFakeCpu{registerA=0xBA,registerY=0xBC} 
                        `shouldBe` getFakeCpu{registerA=0xBA,registerY=0xBA,
                                processorStatus=flags{negative=setNegative 0xBA,zero=setZero 0xBA}}
                    
                            
        describe "Parse.tya" $ do
            it "Transfer Index Y to A" $ do
                let flags = (processorStatus getFakeCpu)
                tya getFakeCpu{registerA=0xC1,registerY=0xC5} 
                        `shouldBe` getFakeCpu{registerA=0xC5,registerY=0xC5,
                                processorStatus=flags{negative=setNegative 0xC5,zero=setZero 0xC5}}
                    
        describe "Parse.dey" $ do
            it "Decrement Index Y" $ do
                let flags = (processorStatus getFakeCpu)
                let newY = (registerY getFakeCpu) - 1 :: Word8
                dey getFakeCpu{registerY=newY + 1} 
                        `shouldBe` getFakeCpu{registerY=newY,
                                processorStatus=flags{negative=setNegative newY,zero=setZero newY}}
                        
        describe "Parse.iny" $ do
            it "Increment Index Y" $ do
                let flags = (processorStatus getFakeCpu)
                let newY = (registerY getFakeCpu) + 1 :: Word8
                iny getFakeCpu{registerY=newY - 1} 
                        `shouldBe` getFakeCpu{registerY=newY,
                                processorStatus=flags{negative=setNegative newY,zero=setZero newY}}


                    
                         