module Opcode where

    import Data.Word (Word8,Word16)

    type Address = Word16
    type Data = Word8

    data AddressMode = 
                          None
                        | Imm Data
                        | Zpg Address
                        | ZpX Address
                        | Abs Address
                        | AbX Address
                        | AbY Address
                        | InX Address
                        | InY Address
                        deriving (Eq, Show)


                        
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
    

   