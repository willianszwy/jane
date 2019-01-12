module OpcodeSpec where
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Opcode

spec :: Spec
spec =  do
    describe "Opcode.setNegative" $ do
        it "returns True when given a negative number input" $ do
            setNegative 0xFF `shouldBe` True
    
        it "returns False when given a positive number input" $ do 
            setNegative 0x7F `shouldBe` False
    
    describe "Opcode.setZero" $ do 
        it "returns True when given a zero input" $
            setZero 0x00 `shouldBe` True
        
        it "return False when given a not zero input" $ 
            setZero 0x01 `shouldBe` False
    
    describe "Opcode.setOverflow" $ do
        it "returns False when given a input overflow signed operations" $ 
            setOverflow 0x01 0x01 (0x01 + 0x01) `shouldBe` False
    
        it "returns False when given a input overflow signed operations" $ 
            setOverflow 0x01 0xFF (0x01 + 0xFF) `shouldBe` False
        
        it "returns True when given a input overflow signed operations" $ 
            setOverflow 0x7F 0x01 (0x7F + 0x01) `shouldBe` True
        
        it "returns True when given a input overflow flag on signed operations" $ 
            setOverflow 0x80 0xFF (0x80 + 0xFF) `shouldBe` True

    describe "Opcode.setCarry" $ do
        it "returns False when given a input carry flag on unsigned operations" $ 
            setCarry (0x01 + 0x01) `shouldBe` False

        it "returns True when given a input carry flag on unsigned operations" $ 
            setCarry (0xFF + 0xFF) `shouldBe` True
