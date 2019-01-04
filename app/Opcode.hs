module Opcode where

    import Data.Word

    data AddressingMode = Immediate Word16
                        | ZeroPage Word16
                        | ZeroPageX Word16
                        | Absolute Word16
                        | AbsoluteX Word16
                        | AbsoluteY Word16
                        | IndirectX Word16
                        | IndirectY Word16
                        deriving (Show)




    data Opcade =  ADC  
             | AND 
             | ASL 
             | BCC 
             | BCS 
             | BEQ | BIT | BMI | BNE | BPL | BRK | BVC | BVS | CLC | CLD | CLI | CLV | CMP | CPX | CPY
             | DEC | DEX | DEY | EOR | INC | INX | INY | JMP | JSR | LDA | LDX | LDY | LSR | NOP | ORA
             | PHA | PHP | PLA | PLP | ROL | ROR | RTI | RTS | SBC | SEC | SED | SEI | STA | STX | STY
             | TAX | TAY | TSX | TXA | TXS | TYA 
             deriving (Show)
    

   