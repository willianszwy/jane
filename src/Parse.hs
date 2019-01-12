{-# LANGUAGE NamedFieldPuns #-}
module Parse where

    import Data.Word (Word8,Word16)
    import Data.Bits ((.|.),(.&.),shift,shiftR)
    import Opcode as Op
    import VRam as VR
    import Cpu

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
            0x10 -> BPL None
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
            0x30 -> BMI None
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
            0x50 -> BVC None
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
            0x70 -> BVS None
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
            0x90 -> BCC None
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
            0xB0 -> BCS (Abs (getWord16FromChunk chunk)) 
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
            0xD0 -> BNE None
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
            0xF0 -> BEQ None
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