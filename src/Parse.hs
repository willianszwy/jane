{-# LANGUAGE NamedFieldPuns #-}
module Parse where

    import Data.Word (Word8,Word16)
    import Data.Bits ((.|.),(.&.),shift,shiftR,xor,complement)
    import Data.Maybe(fromJust)
    import Opcode as Op
    import VRam as VR
    import Cpu
    import Flags

    data Chunk = Empty | Chunk { code :: Word8, lower :: Word8, upper :: Word8 } deriving (Eq,Show)

    getWord16 :: Word8 -> Word8 -> Word16 
    getWord16 upper lower = ( shift (fromIntegral upper) 8) .|. (fromIntegral lower)

    getWord8 :: Word16 -> (Word8, Word8)
    getWord8 x = ( fromIntegral (x .&. 0xFF), fromIntegral $ (x .&. 0xFF00) `shiftR` 8 )

    getWord16FromChunk :: Chunk -> Word16
    getWord16FromChunk Empty = error "Chunk Empty"
    getWord16FromChunk Chunk {upper, lower} = getWord16 upper lower

    getChunk :: Word16 -> VRam -> Chunk
    getChunk address vram = 
            let t = VR.slice address (checkRange address) vram in 
            case length t of 
                3 -> Chunk { code = head t, lower = t !! 1, upper = t !! 2 }
                2 -> Chunk { code = head t, lower = t !! 1, upper = 0 }
                1 -> Chunk { code = head t, lower = 0, upper = 0 }
            where                 
                checkRange 0xFFFF = 1
                checkRange 0xFFFE = 2
                checkRange _ = 3
    
    
    parseChunk :: Chunk -> Opcode
    parseChunk chunk =
         case code chunk of
            0x00 -> BRK  None
            0x01 -> ORA (InX (lower chunk)) 
            0x05 -> ORA (Zpg (lower chunk)) 
            0x06 -> ASL (Zpg (lower chunk)) 
            0x08 -> PHP None
            0x09 -> ORA (Imm (lower chunk)) 
            0x0A -> ASL Acc
            0x0D -> ORA (Abs (getWord16FromChunk chunk)) 
            0x0E -> ASL (Abs (getWord16FromChunk chunk)) 
            0x10 -> BPL (Rel (lower chunk))
            0x11 -> ORA (InY (lower chunk)) 
            0x15 -> ORA (ZpX (lower chunk)) 
            0x16 -> ASL (ZpX (lower chunk)) 
            0x18 -> CLC None
            0x19 -> ORA (AbY (getWord16FromChunk chunk)) 
            0x1D -> ORA (AbX (getWord16FromChunk chunk)) 
            0x1E -> ASL (AbX (getWord16FromChunk chunk)) 
            0x20 -> JSR (Abs (getWord16FromChunk chunk)) 
            0x21 -> AND (InX (lower chunk)) 
            0x24 -> BIT (Zpg (lower chunk)) 
            0x25 -> AND (Zpg (lower chunk)) 
            0x26 -> ROL (Zpg (lower chunk)) 
            0x28 -> PLP None
            0x29 -> AND (Imm (lower chunk)) 
            0x2A -> ROL Acc
            0x2C -> BIT (Abs (getWord16FromChunk chunk))  
            0x2D -> AND (Abs (getWord16FromChunk chunk)) 
            0x2E -> ROL (Abs (getWord16FromChunk chunk)) 
            0x30 -> BMI (Rel (lower chunk))
            0x31 -> AND (InY (lower chunk)) 
            0x35 -> AND (ZpX (lower chunk)) 
            0x36 -> ROL (ZpX (lower chunk)) 
            0x38 -> SEC None
            0x39 -> AND (AbY (getWord16FromChunk chunk)) 
            0x3D -> AND (AbX (getWord16FromChunk chunk)) 
            0x3E -> ROL (AbX (getWord16FromChunk chunk)) 
            0x40 -> RTI None
            0x41 -> EOR (InX (lower chunk)) 
            0x45 -> EOR (Zpg (lower chunk)) 
            0x46 -> LSR (Zpg (lower chunk)) 
            0x48 -> PHA None
            0x49 -> EOR (Imm (lower chunk)) 
            0x4A -> LSR Acc
            0x4C -> JMP (Abs (getWord16FromChunk chunk)) 
            0x4D -> EOR (Abs (getWord16FromChunk chunk)) 
            0x4E -> LSR (Abs (getWord16FromChunk chunk)) 
            0x50 -> BVC (Rel (lower chunk))
            0x51 -> EOR (InY (lower chunk)) 
            0x55 -> EOR (ZpX (lower chunk)) 
            0x56 -> LSR (ZpX (lower chunk)) 
            0x58 -> CLI None
            0x59 -> EOR (AbY (getWord16FromChunk chunk)) 
            0x5D -> EOR (AbX (getWord16FromChunk chunk)) 
            0x5E -> LSR (AbX (getWord16FromChunk chunk)) 
            0x60 -> RTS None
            0x61 -> ADC (InX (lower chunk)) 
            0x65 -> ADC (Zpg (lower chunk)) 
            0x66 -> ROR (Zpg (lower chunk)) 
            0x68 -> PLA None
            0x69 -> ADC (Imm (lower chunk)) 
            0x6A -> ROR Acc
            0x6C -> JMP (Ind (getWord16FromChunk chunk)) 
            0x6D -> ADC (Abs (getWord16FromChunk chunk)) 
            0x6E -> ROR (Abs (getWord16FromChunk chunk)) 
            0x70 -> BVS (Rel (lower chunk))
            0x71 -> ADC (InY (lower chunk)) 
            0x75 -> ADC (ZpX (lower chunk)) 
            0x76 -> ROR (ZpX (lower chunk)) 
            0x78 -> SEI None
            0x79 -> ADC (AbY (getWord16FromChunk chunk)) 
            0x7D -> ADC (AbX (getWord16FromChunk chunk)) 
            0x7E -> ROR (AbX (getWord16FromChunk chunk)) 
            0x81 -> STA (InX (lower chunk)) 
            0x84 -> STY (Zpg (lower chunk)) 
            0x85 -> STA (Zpg (lower chunk)) 
            0x86 -> STX (Zpg (lower chunk)) 
            0x88 -> DEY None
            0x8A -> TXA None
            0x8C -> STY (Abs (getWord16FromChunk chunk)) 
            0x8D -> STA (Abs (getWord16FromChunk chunk)) 
            0x8E -> STX (Abs (getWord16FromChunk chunk)) 
            0x90 -> BCC (Rel (lower chunk))
            0x91 -> STA (InY (lower chunk)) 
            0x94 -> STY (ZpX (lower chunk)) 
            0x95 -> STA (ZpX (lower chunk)) 
            0x96 -> STX (ZpY (lower chunk)) 
            0x98 -> TYA None
            0x99 -> STA (AbY (getWord16FromChunk chunk)) 
            0x9A -> TXS None
            0x9D -> STA (AbX (getWord16FromChunk chunk)) 
            0xA0 -> LDY (Imm (lower chunk)) 
            0xA2 -> LDX (Imm (lower chunk)) 
            0xA4 -> LDY (Zpg (lower chunk)) 
            0xA5 -> LDA (Zpg (lower chunk)) 
            0xA6 -> LDX (Zpg (lower chunk)) 
            0xA8 -> TAY None
            0xA9 -> LDA (Imm (lower chunk)) 
            0xAA -> TAX None
            0xAC -> LDY (Abs (getWord16FromChunk chunk)) 
            0xAD -> LDA (Abs (getWord16FromChunk chunk)) 
            0xAE -> LDX (Abs (getWord16FromChunk chunk)) 
            0xB0 -> BCS (Rel (lower chunk)) 
            0xB1 -> LDA (InY (lower chunk)) 
            0xB4 -> LDY (ZpX (lower chunk)) 
            0xB5 -> LDA (ZpX (lower chunk)) 
            0xB6 -> LDA (ZpY (lower chunk)) 
            0xB8 -> CLV None
            0xB9 -> LDA (AbY (getWord16FromChunk chunk)) 
            0xBA -> TSX None 
            0xBC -> LDY (AbX (getWord16FromChunk chunk)) 
            0xBD -> LDA (AbX (getWord16FromChunk chunk)) 
            0xBE -> LDX (AbY (getWord16FromChunk chunk)) 
            0xC0 -> CPY (Imm (lower chunk)) 
            0xC1 -> CMP (InX (lower chunk)) 
            0xC4 -> CPY (Zpg (lower chunk)) 
            0xC5 -> CMP (Zpg (lower chunk)) 
            0xC6 -> DEC (Zpg (lower chunk)) 
            0xC8 -> INY None
            0xC9 -> CMP (Imm (lower chunk)) 
            0xCA -> DEX None
            0xCC -> CPY (Abs (getWord16FromChunk chunk)) 
            0xCD -> CMP (Abs (getWord16FromChunk chunk)) 
            0xCE -> DEC (Abs (getWord16FromChunk chunk)) 
            0xD0 -> BNE (Rel (lower chunk))
            0xD1 -> CMP (InY (lower chunk)) 
            0xD5 -> CMP (ZpX (lower chunk)) 
            0xD6 -> DEC (ZpX (lower chunk)) 
            0xD8 -> CLD None
            0xD9 -> CMP (AbY (getWord16FromChunk chunk)) 
            0xDD -> CMP (AbX (getWord16FromChunk chunk)) 
            0xDE -> DEC (AbX (getWord16FromChunk chunk)) 
            0xE0 -> CPX (Imm (lower chunk)) 
            0xE1 -> SBC (InX (lower chunk)) 
            0xE4 -> CPX (Zpg (lower chunk)) 
            0xE5 -> SBC (Zpg (lower chunk)) 
            0xE6 -> INC (Zpg (lower chunk)) 
            0xE8 -> INX None
            0xE9 -> SBC (Imm (lower chunk)) 
            0xEA -> NOP None
            0xEC -> CPX (Abs (getWord16FromChunk chunk)) 
            0xED -> SBC (Abs (getWord16FromChunk chunk)) 
            0xEE -> INC (Abs (getWord16FromChunk chunk)) 
            0xF0 -> BEQ (Rel (lower chunk))
            0xF1 -> SBC (InY (lower chunk)) 
            0xF5 -> SBC (ZpX (lower chunk)) 
            0xF6 -> INC (ZpX (lower chunk)) 
            0xF8 -> SED None
            0xF9 -> SBC (AbY (getWord16FromChunk chunk)) 
            0xFD -> SBC (AbX (getWord16FromChunk chunk)) 
            0xFE -> INC (AbX (getWord16FromChunk chunk))  

    readAddressMode :: AddressMode -> (Cpu,VRam) -> Maybe Word8
    readAddressMode a (cpu,vram) = 
            case a of
                None  -> Nothing
                Acc   -> Just (registerA cpu)
                Rel x -> Just x
                Imm x -> Just x
                Zpg x -> Just (readRam $ fromIntegral x)
                ZpX x -> Just (readRam $ wraps x (registerX cpu))
                ZpY x -> Just (readRam $ wraps x (registerY cpu))
                Abs x -> Just (readRam x)
                AbX x -> Just (readRam $ x + fromIntegral (registerX cpu))
                AbY x -> Just (readRam $ x + fromIntegral (registerY cpu))
                InX x -> let a = readAddress $ wraps x (registerX cpu) in Just (readRam a)                          
                InY x -> let a = readAddress $ fromIntegral x in Just (readRam $ a + (fromIntegral (registerY cpu) :: Word16) )
                _ -> error "Error Address Mode unknown"
            where 
                readRam :: Word16 -> Word8
                readRam = VR.read vram
                    
                wraps :: Word8 -> Word8 -> Word16
                wraps x y = fromIntegral (x + y) :: Word16

                readAddress x = getWord16 (readRam $ x + 1) (readRam x)



    translateAddressMode :: AddressMode -> (Cpu,VRam) -> Maybe Word16
    translateAddressMode a (cpu,vram) = 
            case a of
                None  -> Nothing
                Acc   -> Nothing
                Imm x -> Nothing
                Zpg x -> Just (fromIntegral x)
                ZpX x -> Just (wraps x (registerX cpu))
                ZpY x -> Just (wraps x (registerY cpu))
                Abs x -> Just (x)
                AbX x -> Just (x + fromIntegral (registerX cpu))
                AbY x -> Just (x + fromIntegral (registerY cpu))
                InX x -> let a = readAddress $ wraps x (registerX cpu) in Just (a)                          
                InY x -> let a = readAddress $ fromIntegral x in Just (a + (fromIntegral (registerY cpu) :: Word16) )
                _ -> error "Error Address Mode unknown"
            where
                readRam :: Word16 -> Word8
                readRam = VR.read vram
                    
                wraps :: Word8 -> Word8 -> Word16
                wraps x y = fromIntegral (x + y) :: Word16

                readAddress x = getWord16 (readRam $ x + 1) (readRam x)

    
    setNegative :: Word8 -> Bool
    setNegative w = (w .&. 0x80) == 0x80
            
    setZero :: Word8 -> Bool
    setZero w = w == 0 
                
    setOverflow :: Word16 -> Word16 -> Word16 -> Bool
    setOverflow a v s = ((a `xor` s) .&. (v `xor` s) .&. 0x80) == 0x80
            
    setCarry :: Word16 -> Bool
    setCarry w = (w .&. 0x100) == 0x100
            
    op_adc :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_adc (ADC a) (cpu,vram) = mathOperation a ((+)) (carry) (cpu,vram)

    op_sbc :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_sbc (SBC a) (cpu,vram) = mathOperation a ((-)) (not . carry) (cpu,vram)


    mathOperation :: AddressMode -> (Word16 -> Word16 -> Word16) -> (Flags -> Bool) -> (Cpu,VRam) -> (Cpu,VRam)
    mathOperation a operator flag (cpu,vram) = 
        let 
            x = fromJust $ readAddressMode a (cpu,vram)
            flags = processorStatus cpu
            c = fromIntegral ( fromEnum $ flag flags) :: Word16
            temp = (fromIntegral (registerA cpu) :: Word16) `operator` (fromIntegral x :: Word16) `operator` c
            newA = fromIntegral (temp .&. 0xFF) :: Word8
        in
            (cpu { registerA = newA, processorStatus = flags {carry = setCarry temp,
                    zero = setZero  newA,
                    negative = setNegative newA,
                    overflow =  setOverflow (fromIntegral $ registerA cpu) (fromIntegral x) temp} }, vram)

    

    
    -- FLAGS 
    op_clc :: Cpu -> Cpu
    op_clc cpu =  let flags = processorStatus cpu in  setFlags cpu (flags {carry=False})

    op_sec :: Cpu -> Cpu
    op_sec cpu =  let flags = processorStatus cpu in  setFlags cpu (flags {carry=True})

    op_cli :: Cpu -> Cpu
    op_cli cpu =  let flags = processorStatus cpu in  setFlags cpu (flags {interrupt=False})

    op_sei :: Cpu -> Cpu
    op_sei cpu =  let flags = processorStatus cpu in  setFlags cpu (flags {interrupt=True})

    op_clv :: Cpu -> Cpu
    op_clv cpu =  let flags = processorStatus cpu in  setFlags cpu (flags {overflow=False})

    op_cld :: Cpu -> Cpu
    op_cld cpu =  let flags = processorStatus cpu in  setFlags cpu (flags {decimal=False})

    op_sed :: Cpu -> Cpu
    op_sed cpu =  let flags = processorStatus cpu in  setFlags cpu (flags {decimal=True})

    -- Store Register
    op_sta :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_sta (STA a) (cpu,vram) = 
        let 
            address = fromJust $ translateAddressMode a (cpu,vram) 
            regA = registerA cpu
        in
            (cpu,VR.write vram address regA)

    op_stx :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_stx (STX a) (cpu,vram) = 
        let 
            address = fromJust $ translateAddressMode a (cpu,vram) 
            regX = registerX cpu
        in
            (cpu,VR.write vram address regX)


    op_sty :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_sty (STY a) (cpu,vram) = 
        let 
            address = fromJust $ translateAddressMode a (cpu,vram) 
            regY = registerY cpu
        in
            (cpu,VR.write vram address regY)

    op_txs :: Cpu -> Cpu
    op_txs cpu = cpu {stackPointer = registerX cpu}

    op_tsx :: Cpu -> Cpu
    op_tsx cpu = cpu {registerX = stackPointer cpu}

    op_pha :: (Cpu,VRam) -> (Cpu,VRam)
    op_pha (cpu,vram) = let sp = stackPointer cpu in 
        (cpu{stackPointer=sp - 1}, VR.write vram (getWord16 0x01 sp) (registerA cpu))

    op_pla :: (Cpu,VRam) -> (Cpu,VRam)
    op_pla (cpu,vram) = 
        let 
            sp = (stackPointer cpu) + 1
            newA = VR.read vram (getWord16 0x01 sp) 
            flags = (processorStatus cpu){negative=setNegative newA,zero=setZero newA}

        in
            (cpu{registerA=newA,stackPointer=sp,processorStatus=flags},vram)

    op_php :: (Cpu,VRam) -> (Cpu,VRam)
    op_php (cpu,vram) = 
        let 
            sp = stackPointer cpu 
            status = compact $ processorStatus cpu 
        in
            (cpu{stackPointer=sp - 1}, VR.write vram (getWord16 0x01 sp) (status))

    
    op_plp :: (Cpu,VRam) -> (Cpu,VRam)
    op_plp (cpu,vram) =
        let
            sp = (stackPointer cpu) + 1
            flags = extract $  VR.read vram (getWord16 0x01 sp) 
        in
            (cpu{processorStatus = flags, stackPointer=sp},vram)
   
    op_tax :: Cpu -> Cpu
    op_tax cpu = 
        let 
            newX = registerA cpu
            flags = (processorStatus cpu){negative=setNegative newX,zero=setZero newX}
        in
            cpu{registerX = newX, processorStatus = flags}

    
    op_txa :: Cpu -> Cpu
    op_txa cpu = 
        let 
            newA = registerX cpu
            flags = (processorStatus cpu){negative=setNegative newA,zero=setZero newA}
        in
            cpu{registerA = newA, processorStatus = flags}


    op_dex :: Cpu -> Cpu
    op_dex cpu = 
        let
            newX = (registerX cpu) - 1
            flags = (processorStatus cpu){negative=setNegative newX,zero=setZero newX}
        in
            cpu{registerX=newX, processorStatus=flags}
            
            
    op_inx :: Cpu -> Cpu
    op_inx cpu = 
        let
            newX = (registerX cpu) + 1
            flags = (processorStatus cpu){negative=setNegative newX,zero=setZero newX}
        in
            cpu{registerX=newX, processorStatus=flags}

           
--------------------------------------------------------------------------------


    op_tay :: Cpu -> Cpu
    op_tay cpu = 
        let 
            newY = registerA cpu
            flags = (processorStatus cpu){negative=setNegative newY,zero=setZero newY}
        in
            cpu{registerY = newY, processorStatus = flags}

    
    op_tya :: Cpu -> Cpu
    op_tya cpu = 
        let 
            newA = registerY cpu
            flags = (processorStatus cpu){negative=setNegative newA,zero=setZero newA}
        in
            cpu{registerA = newA, processorStatus = flags}


    op_dey :: Cpu -> Cpu
    op_dey cpu = 
        let
            newY = (registerY cpu) - 1
            flags = (processorStatus cpu){negative=setNegative newY,zero=setZero newY}
        in
            cpu{registerY=newY, processorStatus=flags}
            
            
    op_iny :: Cpu -> Cpu
    op_iny cpu = 
        let
            newY = (registerY cpu) + 1
            flags = (processorStatus cpu){negative=setNegative newY,zero=setZero newY}
        in
            cpu{registerY=newY, processorStatus=flags}
    

    op_dec :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_dec (DEC x) (cpu,vram) = 
        let 
            address = fromJust $ translateAddressMode x (cpu,vram)
            mem = (fromJust $ readAddressMode x (cpu,vram)) - 1
            flags = (processorStatus cpu){negative=setNegative mem,zero=setZero mem}
        in
            (cpu{processorStatus=flags},VR.write vram address mem)

            
    op_inc :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_inc (INC x) (cpu,vram) = 
        let 
            address = fromJust $ translateAddressMode x (cpu,vram)
            mem = (fromJust $ readAddressMode x (cpu,vram)) + 1
            flags = (processorStatus cpu){negative=setNegative mem,zero=setZero mem}
        in
            (cpu{processorStatus=flags},VR.write vram address mem)

    op_cmp :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_cmp (CMP x) (cpu,vram) = compareRegister registerA x (cpu,vram)

    op_cpx :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_cpx (CPX x) (cpu,vram) = compareRegister registerX x (cpu,vram)

    op_cpy :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_cpy (CPY x) (cpu,vram) = compareRegister registerY x (cpu,vram)

   
    compareRegister :: (Cpu -> Word8) -> AddressMode -> (Cpu,VRam) -> (Cpu,VRam)
    compareRegister reg x (cpu,vram) = 
        let
            mem = fromJust $ readAddressMode x (cpu,vram)
            a = reg cpu
            n = setNegative (a - mem)
            z = a == mem
            c = a >= mem
            flags = (processorStatus cpu){negative=n,zero=z,carry=c}
        in
            (cpu{processorStatus=flags},vram)

    op_and :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_and (AND x) (cpu,vram) = logicalAcumulator (.&.) x (cpu,vram)

    op_ora :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_ora (ORA x) (cpu,vram) = logicalAcumulator (.|.) x (cpu,vram)

    op_eor :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_eor (EOR x) (cpu,vram) = logicalAcumulator (xor) x (cpu,vram)

    logicalAcumulator :: (Word8 -> Word8 -> Word8) -> AddressMode -> (Cpu,VRam) -> (Cpu,VRam)
    logicalAcumulator f x (cpu,vram) =
        let
            mem = fromJust $ readAddressMode x (cpu,vram)
            newA = f mem (registerA cpu)
            n = setNegative newA
            z = setZero newA
            flags = (processorStatus cpu){negative=n,zero=z}
        in
            (cpu{registerA=newA,processorStatus=flags},vram)


    op_asl :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_asl (ASL Acc) (cpu,vram) = 
        let
            temp = shift (fromIntegral (registerA cpu) :: Word16) (1)
            newA = fromIntegral (temp .&. 0xFF) :: Word8
            n = setNegative newA
            z = setZero newA
            c = setCarry temp
            flags = (processorStatus cpu){negative=n,zero=z,carry=c}
        in
            (cpu{registerA=newA,processorStatus=flags},vram)
    op_asl (ASL x) (cpu,vram) =
        let
            mem = fromJust $ readAddressMode x (cpu,vram)
            temp = shift (fromIntegral mem :: Word16) (1)
            new = fromIntegral (temp .&. 0xFF) :: Word8
            n = setNegative new
            z = setZero new
            c = setCarry temp
            flags = (processorStatus cpu){negative=n,zero=z,carry=c}
            address = fromJust $ translateAddressMode x (cpu,vram)
            vram' = VR.write vram address new
        in
            (cpu{processorStatus=flags},vram')



    op_lsr :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_lsr (LSR Acc) (cpu,vram) = 
                let
                    newA = shift (registerA cpu) (-1)
                    n = setNegative newA
                    z = setZero newA
                    c = False
                    flags = (processorStatus cpu){negative=n,zero=z,carry=c}
                in
                    (cpu{registerA=newA,processorStatus=flags},vram)
    op_lsr (LSR x) (cpu,vram) =
                let
                    mem = fromJust $ readAddressMode x (cpu,vram)
                    new = shift mem (-1)
                    n = setNegative new
                    z = setZero new
                    c = False
                    flags = (processorStatus cpu){negative=n,zero=z,carry=c}
                    address = fromJust $ translateAddressMode x (cpu,vram)
                    vram' = VR.write vram address new
                in
                    (cpu{processorStatus=flags},vram')
    
    
    op_rol :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_rol (ROL Acc) (cpu,vram) = 
        let 
            c_old = fromIntegral $ fromEnum (carry (processorStatus cpu)) :: Word16
            temp = c_old .|. (shift (fromIntegral (registerA cpu) :: Word16) (1))
            newA = fromIntegral (temp .&. 0xFF) :: Word8
            n = setNegative newA
            z = setZero newA
            c = setCarry temp
            flags = (processorStatus cpu){negative=n,zero=z,carry=c}
        in
            (cpu{registerA=newA,processorStatus=flags},vram)
    op_rol (ROL x) (cpu,vram) = 
        let 
            mem = fromJust $ readAddressMode x (cpu,vram)
            c_old = fromIntegral $ fromEnum (carry (processorStatus cpu)) :: Word16
            temp = c_old .|. (shift (fromIntegral mem :: Word16) (1))
            new = fromIntegral (temp .&. 0xFF) :: Word8
            n = setNegative new
            z = setZero new
            c = setCarry temp
            flags = (processorStatus cpu){negative=n,zero=z,carry=c}
            address = fromJust $ translateAddressMode x (cpu,vram)
            vram' = VR.write vram address new
        in
            (cpu{processorStatus=flags},vram')



    op_ror :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_ror (ROR Acc) (cpu,vram) = 
        let 
            a = registerA cpu
            c_old = if carry (processorStatus cpu) then 0x80 else 0x0
            newA = c_old .|. (shift a (-1))
            n = setNegative newA
            z = setZero newA
            c = if a .&. 0x1 == 1 then True else False
            flags = (processorStatus cpu){negative=n,zero=z,carry=c}
        in
            (cpu{registerA=newA,processorStatus=flags},vram)
    op_ror (ROR x) (cpu,vram) = 
        let 
            mem = fromJust $ readAddressMode x (cpu,vram)
            c_old = if carry (processorStatus cpu) then 0x80 else 0x0
            new = c_old .|. (shift mem (-1))
            n = setNegative new
            z = setZero new
            c = if mem .&. 0x1 == 1 then True else False
            flags = (processorStatus cpu){negative=n,zero=z,carry=c}
            address = fromJust $ translateAddressMode x (cpu,vram)
            vram' = VR.write vram address new
        in
            (cpu{processorStatus=flags},vram')

    op_bit :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_bit (BIT x) (cpu,vram) = 
        let
            mem = fromJust $ readAddressMode x (cpu,vram)
            v = mem .&. 0x40 == 0x40
            n = mem .&. 0x80 == 0x80
            z = mem .&. (registerA cpu) == 0
            flags = (processorStatus cpu){negative=n,zero=z,overflow=v}
        in
            (cpu{processorStatus=flags},vram)
    
    -- Branch's
    getSignedWord8 :: Word8 -> Word8
    getSignedWord8 src  = if setNegative src then ((complement src) + 0x1 :: Word8) else src

    offsetBranch :: Word16 -> Word8 -> Word16
    offsetBranch pos offset = 
        let off = fromIntegral (getSignedWord8 offset) :: Word16 in
        if setNegative offset then pos - (off - 2) else pos + (off + 2)

    branch :: AddressMode -> (Flags -> Bool) -> Bool -> Cpu -> Cpu
    branch (Rel address) f b cpu = 
        let
            flags = processorStatus cpu
            pc = programCounter cpu
            newPC = if (f flags) == b then offsetBranch pc address 
                 else pc
        in
            (cpu{programCounter=newPC})


    op_bpl :: Opcode -> Cpu -> Cpu
    op_bpl (BPL x) cpu = branch x negative False cpu

    op_bmi :: Opcode -> Cpu -> Cpu
    op_bmi (BMI x) cpu = branch x negative True cpu

    op_bvc :: Opcode -> Cpu -> Cpu
    op_bvc (BVC x) cpu = branch x overflow False cpu

    op_bvs :: Opcode -> Cpu -> Cpu
    op_bvs (BVS x) cpu = branch x overflow True cpu

    op_bcc :: Opcode -> Cpu -> Cpu
    op_bcc (BCC x) cpu = branch x carry False cpu

    op_bcs :: Opcode -> Cpu -> Cpu
    op_bcs (BCS x) cpu = branch x carry True cpu

    op_bne :: Opcode -> Cpu -> Cpu
    op_bne (BNE x) cpu = branch x zero False cpu

    op_beq :: Opcode -> Cpu -> Cpu
    op_beq (BEQ x) cpu = branch x zero True cpu

    -- Jumps

    op_jmp :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_jmp (JMP (Abs address)) (cpu,vram) = (cpu{programCounter=address},vram)
    op_jmp (JMP (Ind address)) (cpu,vram) = 
        let
            (a_lower,a_upper) = getWord8 address
            lower = VR.read vram address
            upper = VR.read vram (getWord16 a_upper (a_lower + 1))
        in
            (cpu{programCounter=getWord16 upper lower},vram)

    
    op_jsr :: Opcode -> (Cpu,VRam) -> (Cpu,VRam)
    op_jsr (JSR (Abs a)) (cpu,vram) = 
        let 
            (lower,upper) = getWord8 $ (programCounter cpu) + 2
            (cpu',vram')= push upper (cpu,vram)
            (cpu1,vram1)= push lower (cpu',vram')
        in
            (cpu1{programCounter=a},vram1)


    op_rts :: (Cpu,VRam) -> (Cpu,VRam)
    op_rts (cpu,vram) = 
        let
            (lower,(cpu',vram'))   = pull  (cpu,vram)
            (upper,(cpu1,vram1))   = pull  (cpu',vram')
            address = (getWord16 upper lower)
        in 
            (cpu1{programCounter=address},vram1)

    op_brk :: (Cpu,VRam) -> (Cpu,VRam)
    op_brk (cpu,vram) = 
        let
            (lower,upper) = getWord8 $ (programCounter cpu) + 2
            (cpu',vram')= push upper (cpu,vram)
            (cpu1,vram1)= push lower (cpu',vram')
            flags = processorStatus cpu1
            (cpu2,vram2) = op_php (cpu1{processorStatus=flags{Flags.break=True}},vram1)
        in
            op_jmp (JMP (Ind 0xFFFE)) (cpu2{processorStatus=flags{interrupt=True,Flags.break=True}},vram2)

    op_rti :: (Cpu,VRam) -> (Cpu,VRam)
    op_rti (cpu,vram) = 
        let
            (cpu1,vram1) = op_plp(cpu,vram)
            (lower,(cpu2,vram2))   = pull  (cpu1,vram1)
            (upper,(cpu3,vram3))   = pull  (cpu2,vram2)
            address = (getWord16 upper lower)
        in 
            (cpu3{programCounter=address},vram3)


    push :: Word8 -> (Cpu,VRam) -> (Cpu,VRam)
    push v (cpu,vram) = 
        let
            sp = stackPointer cpu
            vram' = VR.write vram (getWord16 0x01 sp) v
        in
            (cpu{stackPointer=sp - 1},vram')


    pull :: (Cpu,VRam) -> (Word8,(Cpu,VRam))
    pull (cpu,vram) = 
        let
            sp = (stackPointer cpu) + 1
            value = VR.read vram (getWord16 0x01 sp)
        in
            (value,(cpu{stackPointer=sp},vram))
            