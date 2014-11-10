module Scalpel.ARM (ARMInstruction, Register(..), Shift(..),
    ShifterOperand(..), armLabel)
    where

import Control.Applicative ((<$>), (<*>))
import Data.Binary.Get (bytesRead, getWord32le)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit)
import Data.Word (Word32)
import Text.Printf (printf)

import Scalpel (Address, Condition(..), Instruction(..), Register(..),
    bitsToRegister, showCondition, testZeroBit)


type RawInstruction = Word32

data ShifterOperand =
    Register Register |
    Immediate Int |
    ShiftImmediate Register Shift Int |
    ShiftRegister Register Shift Register
    deriving Show

data Shift = LSL | LSR | ASR | ROR
    deriving Show

data ARMInstruction =
    B Condition Address |
    BL Condition Address |
    BX Condition Register |
    Unknown RawInstruction
    deriving Show

instance Instruction ARMInstruction where
    getInstruction baseAddress = parseInstruction <$> getWord32le <*> getPC
        where getPC = (4 + baseAddress +) . fromIntegral <$> bytesRead

    stop (B AL _) = True
    stop (BX AL _) = True
    stop _ = False

    branchAddress (B _ target) = Just target
    branchAddress (BL _ target) = Just target
    branchAddress _ = Nothing

    showGAS (B cond address) =
        printf "B%s %s" (showCondition cond) (armLabel address)
    showGAS (BL cond address) =
        printf "BL%s %s" (showCondition cond) (armLabel address)
    showGAS (BX cond register) =
        printf "BX%s %s" (showCondition cond) (show register)
    showGAS (Unknown x) = printf ".word 0x%08X  @ unknown" x

-- Parse a raw 32-bit ARM instruction.
parseInstruction :: RawInstruction -> Address -> ARMInstruction
parseInstruction instruction pc =
    if cond /= NV then
        if instruction `testZeroBit` 27 then
            if instruction .&. 0x0FF000F0 == 0x01200010 then
                BX cond (bitsToRegister (instruction .&. 0xF))
            else
                Unknown instruction
        else
            if instruction `testZeroBit` 26 then
                if instruction `testZeroBit` 25 then
                    Unknown instruction
                else
                    parseBranch instruction cond pc
            else
                Unknown instruction
    else
        Unknown instruction
    where cond = toEnum . fromIntegral $ instruction `shiftR` 28 :: Condition

-- Parse a raw B or BL instruction.  It is assumed that the top seven bits
-- indicate that this is indeed a B/BL instruction.
parseBranch :: RawInstruction -> Condition -> Address -> ARMInstruction
parseBranch instruction condition pc =
    if instruction `testZeroBit` 24 then
        B condition target
    else
        BL condition target
    where
        target = pc + offset
        offset = signExtend (instruction .&. 0x00FFFFFF) `shiftL` 2
        signExtend x = if x `testBit` 23 then x .|. 0x3F000000 else x

-- Autogenerate a label based on an address.
armLabel :: Address -> String
armLabel = printf "arm_0x%08X"
