module Opcode where

    import Data.Word (Word8,Word16)
    import Numeric(showHex)
    import Data.Bits((.&.),xor)
    import Flags
    import Cpu
    import VRam

    data AddressMode = 
                          None
                        | Acc
                        | Imm Word8
                        | Zpg Word8
                        | ZpX Word8
                        | ZpY Word8
                        | Abs Word16
                        | AbX Word16
                        | AbY Word16
                        | Ind Word16
                        | InX Word8
                        | InY Word8
                        deriving (Eq)
    
    instance Show AddressMode where
        show (None) = ""
        show (Acc) = "A"
        show (Imm x) = "#$" ++ showHex x ""
        show (Zpg x) = "$" ++ showHex x ""
        show (ZpX x) = "$" ++ showHex x ",X"
        show (ZpY x) = "$" ++ showHex x ",Y"
        show (Abs x) = "$" ++ showHex x ""
        show (AbX x) = "$" ++ showHex x ",X"
        show (AbY x) = "$" ++ showHex x ",Y"
        show (Ind x) = "($" ++ showHex x ")"
        show (InX x) = "($" ++ showHex x ",X)"
        show (InY x) = "($" ++ showHex x "),Y"
                        
    data Opcode =  
               ADC  AddressMode
             | AND  AddressMode
             | ASL  AddressMode
             | BCC  AddressMode
             | BCS  AddressMode
             | BEQ  AddressMode
             | BIT  AddressMode
             | BMI  AddressMode
             | BNE  AddressMode
             | BPL  AddressMode
             | BRK  AddressMode
             | BVC  AddressMode
             | BVS  AddressMode
             | CLC  AddressMode
             | CLD  AddressMode
             | CLI  AddressMode
             | CLV  AddressMode
             | CMP  AddressMode
             | CPX  AddressMode
             | CPY  AddressMode
             | DEC  AddressMode
             | DEX  AddressMode
             | DEY  AddressMode
             | EOR  AddressMode
             | INC  AddressMode
             | INX  AddressMode
             | INY  AddressMode
             | JMP  AddressMode
             | JSR  AddressMode
             | LDA  AddressMode
             | LDX  AddressMode
             | LDY  AddressMode
             | LSR  AddressMode
             | NOP  AddressMode
             | ORA  AddressMode
             | PHA  AddressMode
             | PHP  AddressMode
             | PLA  AddressMode
             | PLP  AddressMode
             | ROL  AddressMode
             | ROR  AddressMode
             | RTI  AddressMode
             | RTS  AddressMode
             | SBC  AddressMode
             | SEC  AddressMode
             | SED  AddressMode
             | SEI  AddressMode
             | STA  AddressMode
             | STX  AddressMode
             | STY  AddressMode
             | TAX  AddressMode
             | TAY  AddressMode
             | TSX  AddressMode
             | TXA  AddressMode
             | TXS  AddressMode
             | TYA  AddressMode
             deriving (Eq,Show)
    

    

    setNegative :: Word8 -> Bool
    setNegative w = (w .&. 0x80) == 0x80

    setZero :: Word8 -> Bool
    setZero w = w == 0 
    
    setOverflow :: Word16 -> Word16 -> Word16 -> Bool
    setOverflow a v s = ((a `xor` s) .&. (v `xor` s) .&. 0x80) == 0x80

    setCarry :: Word16 -> Bool
    setCarry w = (w .&. 0x100) == 0x100

    adc :: Opcode -> (Cpu,VRam) -> Cpu
    adc (ADC (Imm x)) (cpu,vram) = 
        let 
            flags = processorStatus cpu
            c = fromIntegral (fromEnum $ carry flags ) :: Word16 
            temp = (fromIntegral (registerA cpu) :: Word16) + (fromIntegral x :: Word16) + c       
            newA = fromIntegral (temp .&. 0xFF) :: Word8
            in
            cpu { registerA = newA, processorStatus = flags {carry = setCarry temp,
                            zero = setZero  newA,
                            negative = setNegative newA,
                            overflow = setOverflow (fromIntegral $ registerA cpu) (fromIntegral x) temp} } 


