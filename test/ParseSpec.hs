module ParseSpec where
    import Test.Hspec
    import Test.QuickCheck
    import Control.Exception (evaluate)
    
    import Data.Word(Word8,Word16)
    import qualified Data.Vector.Unboxed as V
    import Data.Bits as B
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

        describe "Parse.op_adc" $ do
            it "Add Memory to Accumulator with Carry" $ do
                let flags = processorStatus getFakeCpu
                op_adc (Imm 0xFF) (getFakeCpu { registerA = 0xFF},getFakeVRam) `shouldBe` 
                 (getFakeCpu{ registerA = 0xFE,processorStatus= flags {carry=True,negative=True}},getFakeVRam)

        
        describe "Parse.op_sbc" $ 
            it "Subtract Memory from Accumulator with Borrow" $ do
                let flags = (processorStatus getFakeCpu){zero=True,carry=False}
                op_sbc (Imm 0x01) (getFakeCpu{registerA=0x02},getFakeVRam) 
                    `shouldBe` (getFakeCpu{registerA=0x00,processorStatus=flags},getFakeVRam)

        describe "Parse.op_clc" $ do
            it "cleans the carry flag" $ do
                let flags = processorStatus getFakeCpu
                op_clc getFakeCpu {processorStatus = flags { carry=False}}
                  `shouldBe` getFakeCpu {processorStatus = flags { carry=False}}

        describe "Parse.op_sec" $ do
            it "set the carry flag" $ do
                let flags = processorStatus getFakeCpu
                op_sec getFakeCpu `shouldBe` getFakeCpu {processorStatus = flags { carry=True}}

        describe "Parse.op_cli" $ do
            it "cleans the interrupt flag" $ do
                let flags = processorStatus getFakeCpu
                op_cli getFakeCpu {processorStatus = flags { interrupt=False}}
                    `shouldBe` getFakeCpu {processorStatus = flags { interrupt=False}}
        
        describe "Parse.op_sei" $ do
            it "set the interrupt flag" $ do
                let flags = processorStatus getFakeCpu
                op_sei getFakeCpu `shouldBe` getFakeCpu {processorStatus = flags { interrupt=True}}

        describe "Parse.op_clv" $ do
            it "cleans the overflow flag" $ do
                let flags = processorStatus getFakeCpu
                op_clv getFakeCpu {processorStatus = flags { overflow=False}}
                    `shouldBe` getFakeCpu {processorStatus = flags { overflow=False}}

        describe "Parse.op_cld" $ do
            it "cleans the decimal flag" $ do
                let flags = processorStatus getFakeCpu
                op_cld getFakeCpu {processorStatus = flags { decimal=False}}
                    `shouldBe` getFakeCpu {processorStatus = flags { decimal=False}}
                    
        describe "Parse.op_sed" $ do
            it "set the decimal flag" $ do
                let flags = processorStatus getFakeCpu
                op_sed getFakeCpu `shouldBe` getFakeCpu {processorStatus = flags { decimal=True}}

        describe "Parse.op_sta" $ do
            it "Store Accumulator in Memory" $ do
                let vram = VRam.write getFakeVRam 0xAA 0xA1
                op_sta (Zpg 0xAA) (getFakeCpu {registerA = 0xA1},getFakeVRam) 
                    `shouldBe` (getFakeCpu {registerA = 0xA1},vram)

        describe "Parse.op_stx" $ do
            it "Store Index X in Memory" $ do
                    let vram = VRam.write getFakeVRam 0xBB 0xA2
                    op_stx (Zpg 0xBB) (getFakeCpu {registerX = 0xA2},getFakeVRam) 
                        `shouldBe` (getFakeCpu {registerX = 0xA2},vram)

        describe "Parse.op_sty" $ do
            it "Store Index Y in Memory" $ do
                    let vram = VRam.write getFakeVRam 0xCC 0xA3
                    op_sty (Zpg 0xCC) (getFakeCpu {registerY = 0xA3},getFakeVRam) 
                        `shouldBe` (getFakeCpu {registerY = 0xA3},vram)

        -- Stack

        describe "Parse.op_txs" $ do
            it "Transfer X to Stack Pointer" $ do
                op_txs (getFakeCpu {registerX = 0xBB, stackPointer = 0xFF})
                    `shouldBe` getFakeCpu {registerX = 0xBB, stackPointer = 0xBB}
        
        describe "Parse.op_tsx" $ do
            it "Transfer Stack Pointer to X" $ do
                op_tsx (getFakeCpu {registerX = 0xBB, stackPointer = 0xFF})
                    `shouldBe` getFakeCpu {registerX = 0xFF, stackPointer = 0xFF}
                        
        describe "Parse.op_pha" $ do
            it "Push Accumulator to Stack" $ do
                let vram = VRam.write getFakeVRam 0x1FF 0xCC
                op_pha (getFakeCpu{registerA = 0xCC, stackPointer = 0xFF},getFakeVRam) 
                    `shouldBe` (getFakeCpu{registerA = 0xCC, stackPointer = 0xFE},vram)

            it "Push Accumulator to Stack should overflow" $ do
                let vram = VRam.write getFakeVRam 0x100 0xDD
                op_pha (getFakeCpu{registerA = 0xDD, stackPointer = 0x00},getFakeVRam) 
                    `shouldBe` (getFakeCpu{registerA = 0xDD, stackPointer = 0xFF},vram)

        describe "Parse.op_pla" $ do
            it "Pull Stack to Acummulator" $ do
                let vram = VRam.write getFakeVRam 0x1FE 0xA1
                let flags = (processorStatus getFakeCpu){negative = setNegative 0xA1, zero = setZero 0xA1}
                op_pla (getFakeCpu{registerA = 0x00, stackPointer = 0xFD},vram) 
                    `shouldBe` (getFakeCpu{registerA = 0xA1, stackPointer = 0xFE,processorStatus=flags},vram)

            it "Push Accumulator to Stack should overflow" $ do
                let vram = VRam.write getFakeVRam 0x100 0xD1
                let flags = (processorStatus getFakeCpu){negative = setNegative 0xD1, zero= setZero 0xD1}
                op_pla (getFakeCpu{registerA = 0x00, stackPointer = 0xFF},vram) 
                    `shouldBe` (getFakeCpu{registerA = 0xD1, stackPointer = 0x00, processorStatus=flags},vram)
       
        describe "Parse.op_php" $ do
            it "Push Processor Status to Stack" $ do
                let flags = (processorStatus getFakeCpu){carry=True,negative=True}
                let status = compact flags
                let vram = VRam.write getFakeVRam 0x1FD status
                op_php (getFakeCpu{processorStatus=flags},getFakeVRam)
                    `shouldBe` (getFakeCpu{processorStatus=flags,stackPointer = 0xFC},vram)
                        
        describe "Parse.op_plp" $ do 
            it "Pull Stack to Processor Status" $ do
                let flags = (processorStatus getFakeCpu){carry=True,interrupt=True}
                let status = compact flags 
                let vram = VRam.write getFakeVRam 0x1FE status
                op_plp (getFakeCpu{stackPointer=0xFD},vram) 
                    `shouldBe` (getFakeCpu{stackPointer=0xFE,processorStatus=flags},vram)
        

        describe "Parse.op_tax" $ do
            it "Transfer Accumulator to X" $ do
                let flags = (processorStatus getFakeCpu)
                op_tax getFakeCpu{registerA=0xBA,registerX=0xBC} 
                    `shouldBe` getFakeCpu{registerA=0xBA,registerX=0xBA,
                        processorStatus=flags{negative=setNegative 0xBA,zero=setZero 0xBA}}

        
        describe "Parse.op_txa" $ do
            it "Transfer Index X to A" $ do
                let flags = (processorStatus getFakeCpu)
                op_txa getFakeCpu{registerA=0xC1,registerX=0xC5} 
                   `shouldBe` getFakeCpu{registerA=0xC5,registerX=0xC5,
                        processorStatus=flags{negative=setNegative 0xC5,zero=setZero 0xC5}}

        describe "Parse.op_dex" $ do
            it "Decrement Index X" $ do
                let flags = (processorStatus getFakeCpu)
                let newX = (registerX getFakeCpu) - 1 :: Word8
                op_dex getFakeCpu{registerX=newX + 1} 
                  `shouldBe` getFakeCpu{registerX=newX,
                        processorStatus=flags{negative=setNegative newX,zero=setZero newX}}
    
        describe "Parse.op_inx" $ do
            it "Increment Index X" $ do
                let flags = (processorStatus getFakeCpu)
                let newX = (registerX getFakeCpu) + 1 :: Word8
                op_inx getFakeCpu{registerX=newX - 1} 
                    `shouldBe` getFakeCpu{registerX=newX,
                            processorStatus=flags{negative=setNegative newX,zero=setZero newX}}


        describe "Parse.op_tay" $ do
            it "Transfer Accumulator to Y" $ do
                let flags = (processorStatus getFakeCpu)
                op_tay getFakeCpu{registerA=0xBA,registerY=0xBC} 
                        `shouldBe` getFakeCpu{registerA=0xBA,registerY=0xBA,
                                processorStatus=flags{negative=setNegative 0xBA,zero=setZero 0xBA}}
                    
                            
        describe "Parse.op_tya" $ do
            it "Transfer Index Y to A" $ do
                let flags = (processorStatus getFakeCpu)
                op_tya getFakeCpu{registerA=0xC1,registerY=0xC5} 
                        `shouldBe` getFakeCpu{registerA=0xC5,registerY=0xC5,
                                processorStatus=flags{negative=setNegative 0xC5,zero=setZero 0xC5}}
                    
        describe "Parse.op_dey" $ do
            it "Decrement Index Y" $ do
                let flags = (processorStatus getFakeCpu)
                let newY = (registerY getFakeCpu) - 1 :: Word8
                op_dey getFakeCpu{registerY=newY + 1} 
                        `shouldBe` getFakeCpu{registerY=newY,
                                processorStatus=flags{negative=setNegative newY,zero=setZero newY}}
                        
        describe "Parse.op_iny" $ do
            it "Increment Index Y" $ do
                let flags = (processorStatus getFakeCpu)
                let newY = (registerY getFakeCpu) + 1 :: Word8
                op_iny getFakeCpu{registerY=newY - 1} 
                        `shouldBe` getFakeCpu{registerY=newY,
                                processorStatus=flags{negative=setNegative newY,zero=setZero newY}}

       
        describe "Parse.op_dec" $ do
            it "Decrement Memory by One" $ do
                let value = (0x0A :: Word8)
                let vram = VRam.write getFakeVRam 0xBC value
                let vram' = VRam.write getFakeVRam 0xBC (value - 1)
                let flags = (processorStatus getFakeCpu){negative=setNegative (value - 1),zero=setZero (value - 1)}
                op_dec (Abs 0x00BC) (getFakeCpu,vram) `shouldBe` (getFakeCpu,vram')


        describe "Parse.op_inc" $ do
            it "Increment Memory by One" $ do
                let value = (0x0B :: Word8)
                let vram = VRam.write getFakeVRam 0xBD value
                let vram' = VRam.write getFakeVRam 0xBD (value + 1)
                let flags = (processorStatus getFakeCpu){negative=setNegative (value + 1),zero=setZero (value + 1)}
                op_inc (Abs 0x00BD) (getFakeCpu,vram) `shouldBe` (getFakeCpu,vram')

        
        describe "Parse.op_cmp" $ do
            it "Compare Memory with Accumulator" $ do
                let flags = (processorStatus getFakeCpu){negative=True}
                op_cmp (Imm 0x80) (getFakeCpu{registerA=0x7F},getFakeVRam)
                    `shouldBe` (getFakeCpu{registerA=0x7F,processorStatus=flags},getFakeVRam)


        describe "Parse.op_cpx" $ do
            it "Compare Memory and Index X" $ do
                let flags = (processorStatus getFakeCpu){negative=True}
                op_cpx (Imm 0x80) (getFakeCpu{registerX=0x7F},getFakeVRam)
                    `shouldBe` (getFakeCpu{registerX=0x7F,processorStatus=flags},getFakeVRam)

                    
        describe "Parse.op_cpy" $ do
            it "Compare Memory and Index Y" $ do
                let flags = (processorStatus getFakeCpu){negative=True}
                op_cpy (Imm 0x80) (getFakeCpu{registerY=0x7F},getFakeVRam)
                    `shouldBe` (getFakeCpu{registerY=0x7F,processorStatus=flags},getFakeVRam)


        describe "Parse.op_and" $ do
            it "AND Memory with Accumulator" $ do
                let res = (0x80 :: Word8) B..&. (0x05 :: Word8)
                let flags = (processorStatus getFakeCpu){negative=setNegative res,zero=setZero res}
                op_and (Imm 0x80) (getFakeCpu{registerA=0x05},getFakeVRam)
                    `shouldBe` (getFakeCpu{registerA=res,processorStatus=flags},getFakeVRam)


        describe "Parse.op_ora" $ do
            it "OR Memory with Accumulator" $ do
                let res = (0x80 :: Word8) B..|. (0x05 :: Word8)
                let flags = (processorStatus getFakeCpu){negative=setNegative res,zero=setZero res}
                op_ora (Imm 0x80) (getFakeCpu{registerA=0x05},getFakeVRam)
                    `shouldBe` (getFakeCpu{registerA=res,processorStatus=flags},getFakeVRam)


        describe "Parse.op_eor" $ do
            it "XOR Memory with Accumulator" $ do
                let res = B.xor (0x80 :: Word8) (0x05 :: Word8)
                let flags = (processorStatus getFakeCpu){negative=setNegative res,zero=setZero res}
                op_eor (Imm 0x80) (getFakeCpu{registerA=0x05},getFakeVRam)
                    `shouldBe` (getFakeCpu{registerA=res,processorStatus=flags},getFakeVRam)
            
       
        describe "Parse.op_asl" $ do
            it "Shift Left One Bit (Accumulator)" $ do
                op_asl (Acc) (getFakeCpu{registerA=0x1},getFakeVRam) `shouldBe` (getFakeCpu{registerA=0x2},getFakeVRam)

            it "Shift Left One Bit (Memory)" $ do
                let vram = VRam.write getFakeVRam 0xBD 0x01
                let vram'= VRam.write getFakeVRam 0xBD 0x02
                op_asl (Zpg 0xBD) (getFakeCpu,vram)
                    `shouldBe` (getFakeCpu,vram')


        describe "Parse.op_lsr" $ do
            it "Shift Right One Bit (Accumulator)" $ do
                op_lsr (Acc) (getFakeCpu{registerA=0x2},getFakeVRam) `shouldBe` (getFakeCpu{registerA=0x1},getFakeVRam)
            
            it "Shift Right One Bit (Memory)" $ do
                let vram = VRam.write getFakeVRam 0xBD 0x02
                let vram'= VRam.write getFakeVRam 0xBD 0x01
                op_lsr (Zpg 0xBD) (getFakeCpu,vram)
                    `shouldBe` (getFakeCpu,vram')
            

        describe "Parse.op_rol" $ do
            it "Rotate One Bit Left (Accumulator) with Carry True" $ do
                let flags = (processorStatus getFakeCpu){carry=True,negative=True}
                op_rol (Acc) (getFakeCpu{registerA=0xFF,processorStatus=flags},getFakeVRam) 
                    `shouldBe` (getFakeCpu{registerA=0xFF,processorStatus=flags},getFakeVRam)

            it "Rotate One Bit Left (Accumulator) with Carry False" $ do
                let flags = (processorStatus getFakeCpu){carry=True,negative=True}
                op_rol (Acc) (getFakeCpu{registerA=0xFF},getFakeVRam) 
                    `shouldBe` (getFakeCpu{registerA=0xFE,processorStatus=flags},getFakeVRam)

            it "Rotate One Bit Left (Memory) with Carry True" $ do
                let vram = VRam.write getFakeVRam 0xBD 0xBD
                let vram'= VRam.write getFakeVRam 0xBD 0x7B
                let flags = (processorStatus getFakeCpu){carry=True,negative=False}
                op_rol (Zpg 0xBD) (getFakeCpu{processorStatus=flags},vram) 
                    `shouldBe` (getFakeCpu{processorStatus=flags},vram')
        
            it "Rotate One Bit Left (Memory) with Carry False" $ do
                let vram = VRam.write getFakeVRam 0xBD 0xBD
                let vram'= VRam.write getFakeVRam 0xBD 0x7A
                let flags = (processorStatus getFakeCpu){carry=True,negative=False}
                op_rol (Zpg 0xBD) (getFakeCpu,vram) 
                    `shouldBe` (getFakeCpu{processorStatus=flags},vram')



        describe "Parse.op_ror" $ do
            it "Rotate One Bit Right (Memory or Accumulator) with Carry True" $ do
                let flags = (processorStatus getFakeCpu){carry=True,negative=True}
                op_ror (Acc) (getFakeCpu{registerA=0xFF,processorStatus=flags},getFakeVRam) 
                    `shouldBe` (getFakeCpu{registerA=0xFF,processorStatus=flags},getFakeVRam)
            
            it "Rotate One Bit Right (Memory or Accumulator) with Carry False" $ do
                let flags = (processorStatus getFakeCpu){carry=True,negative=False}
                op_ror (Acc) (getFakeCpu{registerA=0xFF},getFakeVRam) 
                    `shouldBe` (getFakeCpu{registerA=0x7F,processorStatus=flags},getFakeVRam)

            it "Rotate One Bit Right (Memory or Accumulator) with Carry True" $ do
                let vram = VRam.write getFakeVRam 0xBD 0xCD
                let vram'= VRam.write getFakeVRam 0xBD 0xE6
                let flags = (processorStatus getFakeCpu){carry=True,negative=True}
                op_ror (Zpg 0xBD) (getFakeCpu{processorStatus=flags},vram) 
                    `shouldBe` (getFakeCpu{processorStatus=flags},vram')
                    
            it "Rotate One Bit Right (Memory or Accumulator) with Carry False" $ do
                let vram = VRam.write getFakeVRam 0xBD 0xCD
                let vram'= VRam.write getFakeVRam 0xBD 0x66
                let flags = (processorStatus getFakeCpu){carry=True,negative=False}
                op_ror (Zpg 0xBD) (getFakeCpu,vram) 
                    `shouldBe` (getFakeCpu{processorStatus=flags},vram')
                         

        describe "Parse.op_bit" $ do
            it "Test Bits in Memory with Accumulator" $ do
                let vram = VRam.write getFakeVRam 0x0C 0xC2
                let flags = (processorStatus getFakeCpu){overflow=True,negative=True,zero=False}
                op_bit (Zpg 0x0C) (getFakeCpu{registerA=0xA1},vram) `shouldBe` (getFakeCpu{registerA=0xA1,processorStatus=flags},vram)

        describe "Parse.getSignedWord8" $ do
            it "Calculate 2's compliments of a Word8" $ do
                getSignedWord8 0xFF `shouldBe` 0x01
                getSignedWord8 0x80 `shouldBe` 128


        describe "Parse.offsetBranch" $ do
            it "Returns de new Program Counter address giving a offset" $ do
                offsetBranch 0x0004 0x02 `shouldBe` 0x0008

        describe "Parse.op_bcc" $ do
            it "Branch on carry clear | Carry = False " $ do
                let pc = programCounter getFakeCpu
                op_bcc (Rel 0x02) getFakeCpu 
                    `shouldBe` (getFakeCpu{programCounter = pc + 4})
        
        describe "Parse.op_bcc" $ do
            it "Branch on carry clear | Carry = True" $ do
                let pc = programCounter getFakeCpu
                let flags = (processorStatus getFakeCpu){carry=True}
                op_bcc (Rel 0x02) getFakeCpu{processorStatus=flags} 
                        `shouldBe` (getFakeCpu{processorStatus=flags,programCounter = pc})


        describe "Parse.op_bcs" $ do
            it "Branch on carry set | Carry = True " $ do
                let pc = programCounter getFakeCpu
                let flags = (processorStatus getFakeCpu){carry=True}
                op_bcs (Rel 0x02) getFakeCpu{processorStatus=flags}
                    `shouldBe` (getFakeCpu{processorStatus=flags,programCounter = pc + 4})
                        
        describe "Parse.op_bcs" $ do
            it "Branch on carry set | Carry = False" $ do
                let pc = programCounter getFakeCpu
                let flags = (processorStatus getFakeCpu){carry=False}
                op_bcs (Rel 0x02) getFakeCpu{processorStatus=flags} 
                    `shouldBe` (getFakeCpu{processorStatus=flags,programCounter = pc})

        describe "Parse.op_beq" $ do
            it "Branch on equal Zero True" $ do
                let pc = programCounter getFakeCpu
                let flags = (processorStatus getFakeCpu){zero=True}
                op_beq (Rel 0x02) getFakeCpu{processorStatus=flags} 
                    `shouldBe` (getFakeCpu{processorStatus=flags,programCounter = pc + 4})

        describe "Parse.op_beq" $ do
            it "Branch on equal Zero False" $ do
                let pc = programCounter getFakeCpu
                let flags = (processorStatus getFakeCpu){zero=False}
                op_beq (Rel 0x02) getFakeCpu{processorStatus=flags} 
                    `shouldBe` (getFakeCpu{processorStatus=flags,programCounter = pc })

        describe "Parse.op_bmi" $ do
            it "Branch on Minus Negative True" $ do
                let pc = programCounter getFakeCpu
                let flags = (processorStatus getFakeCpu){negative=True}
                op_bmi (Rel 0x02) getFakeCpu{processorStatus=flags} 
                    `shouldBe` (getFakeCpu{processorStatus=flags,programCounter = pc + 4 })

        describe "Parse.op_bmi" $ do
            it "Branch on Minus Negative False" $ do
                let pc = programCounter getFakeCpu
                let flags = (processorStatus getFakeCpu){negative=False}
                op_bmi (Rel 0x02) getFakeCpu{processorStatus=flags} 
                    `shouldBe` (getFakeCpu{processorStatus=flags,programCounter = pc  })


        describe "Parse.op_bne" $ do
            it "Branch on not Equal Zero False" $ do
                let pc = programCounter getFakeCpu
                let flags = (processorStatus getFakeCpu){zero=False}
                op_bne (Rel 0x02) getFakeCpu{processorStatus=flags} 
                    `shouldBe` (getFakeCpu{processorStatus=flags,programCounter = pc + 4 })
            
        describe "Parse.op_bne" $ do
            it "Branch on not Equal Zero True" $ do
                let pc = programCounter getFakeCpu
                let flags = (processorStatus getFakeCpu){zero=True}
                op_bne (Rel 0x02) getFakeCpu{processorStatus=flags} 
                    `shouldBe` (getFakeCpu{processorStatus=flags,programCounter = pc  })

        describe "Parse.op_bpl" $ do
            it "Branch on Plus negative False" $ do
                let pc = programCounter getFakeCpu
                let flags = (processorStatus getFakeCpu){negative=False}
                op_bpl (Rel 0x02) getFakeCpu{processorStatus=flags} 
                    `shouldBe` (getFakeCpu{processorStatus=flags,programCounter = pc + 4 })
                        
        describe "Parse.op_bpl" $ do
            it "Branch on not Plus negative True" $ do
                let pc = programCounter getFakeCpu
                let flags = (processorStatus getFakeCpu){negative=True}
                op_bpl (Rel 0x02) getFakeCpu{processorStatus=flags} 
                    `shouldBe` (getFakeCpu{processorStatus=flags,programCounter = pc  })


        describe "Parse.op_bvc" $ do
            it "Branch on Overflow False" $ do
                let pc = programCounter getFakeCpu
                let flags = (processorStatus getFakeCpu){overflow=False}
                op_bvc (Rel 0x02) getFakeCpu{processorStatus=flags} 
                    `shouldBe` (getFakeCpu{processorStatus=flags,programCounter = pc + 4 })
                                    
        describe "Parse.op_bvc" $ do
            it "Branch on Overflow True" $ do
                let pc = programCounter getFakeCpu
                let flags = (processorStatus getFakeCpu){overflow=True}
                op_bvc (Rel 0x02) getFakeCpu{processorStatus=flags} 
                    `shouldBe` (getFakeCpu{processorStatus=flags,programCounter = pc  })

        describe "Parse.op_jmp" $ do
            it "Jump to New Location Indirect" $ do
                let vram  = VRam.write getFakeVRam 0x01FF 0x80
                let vram' = VRam.write vram 0x0100 0x02
                op_jmp (Ind 0x01FF) (getFakeCpu,vram') 
                    `shouldBe` (getFakeCpu{programCounter=0x0280},vram')

            it "Jump to New Location Absolute" $ do
                op_jmp (Abs 0x01FF) (getFakeCpu,getFakeVRam) 
                    `shouldBe` (getFakeCpu{programCounter=0x01FF},getFakeVRam)


        describe "Parse.op_jsr" $ do
            it "Jump to New Location Saving Return Address" $ do
                let vram = VRam.write getFakeVRam 0x01FD 0x01
                let vram' = VRam.write vram 0x01FC 0x02
                op_jsr (Abs 0x0110) (getFakeCpu{programCounter=0x0100},getFakeVRam) `shouldBe` 
                        (getFakeCpu{programCounter=0x0110,stackPointer=0xFB},vram')


        describe "Parse.op_rts" $ do
            it "Return from Subroutine" $ do
                let pc = programCounter getFakeCpu
                let (cpu,vram) = op_jsr (Abs 0x0100) (getFakeCpu,getFakeVRam)
                op_rts (cpu,vram) `shouldBe` (getFakeCpu{programCounter=pc + 2},vram)
            
            

        describe "Parse.push" $ do
            it "Push data on stack" $ do
                let sp = stackPointer getFakeCpu
                let vram = VRam.write getFakeVRam (getWord16 0x01 sp) 0x10
                push 0x10 (getFakeCpu, getFakeVRam) `shouldBe` (getFakeCpu{stackPointer=sp - 1},vram)


        describe "Parse.pull" $ do
            it "Pull data from stack" $ do
                let (cpu,vram) =  push 0x10 (getFakeCpu, getFakeVRam)
                let sp = stackPointer cpu
                pull (cpu,vram) `shouldBe` (0x10,(cpu{stackPointer= sp + 1},vram))


        describe "Parse.brk" $ do
            it "Force Break" $ do
                let vram = VRam.init {
                    ram = RAM (V.replicate 0x800 0xFF),
                    ppu = PPU (V.empty),
                    apu = APU (V.empty),
                    openBus = OPB (V.empty),
                    sram = SRM (V.empty),
                    programRom = PRG (V.replicate 0x8000 0)
                }
                let flags = processorStatus getFakeCpu
                let sp = stackPointer getFakeCpu - 3
                let vram1 = VRam.write vram 0x1FD  0
                let vram2 = VRam.write vram1 0x1FC 54
                let vram3 = VRam.write vram2 0x1FB 48
                op_brk (getFakeCpu, vram) 
                    `shouldBe` (getFakeCpu{processorStatus=flags{Flags.break=True,interrupt=True}
                                ,stackPointer=sp,programCounter=0x0}, vram3)
        
        describe "Parse.rti" $ do
            it "Return from Interrupt" $ do
                let flags = processorStatus getFakeCpu
                let sp = stackPointer getFakeCpu - 3
                let vram = VRam.write getFakeVRam 0x1FD  0
                let vram1 = VRam.write vram 0x1FC 54
                let vram2 = VRam.write vram1 0x1FB 48
                op_rti (getFakeCpu{stackPointer=sp},vram2)
                    `shouldBe` (getFakeCpu{processorStatus=flags{Flags.break=True},programCounter=54},vram2)