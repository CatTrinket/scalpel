module Scalpel.Thumb (Instruction, branchAddress, disassembleSection, label,
    printInstructions)
    where

import Control.Applicative ((<$>), (<*>))
import Data.Binary.Get (Get, bytesRead, getWord16le)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit)
import Data.Word (Word16)
import Text.Printf (printf)

import Scalpel (Address, Condition(..))


-- Data types

type RawInstruction = Word16

data Instruction =
    B1 Condition Address |
    B2 Address |
    Unknown RawInstruction
    deriving Show


-- Parsing instructions

-- Parse a raw 16-bit Thumb instruction.
parseInstruction :: RawInstruction -> Address -> Instruction
parseInstruction instruction pc =
    if instruction .&. 0xF000 == 0xD000 then
        if instruction .&. 0x0F00 == 0x0F00 then
            -- SWI
            Unknown instruction
        else
            parseB1 instruction pc
    else if instruction .&. 0xF800 == 0xE000 then
        parseB2 instruction pc
    else
        Unknown instruction

-- Parse a conditional B instruction.
parseB1 :: RawInstruction -> Address -> Instruction
parseB1 instruction pc =
    if condition == AL then
        Unknown instruction
    else
        B1 condition address
    where
        condition = toEnum (fromIntegral conditionBits) :: Condition
        conditionBits = (instruction .&. 0x0F00) `shiftR` 8
        address = pc + offset
        offset = signExtend (fromIntegral offsetBits) `shiftL` 1 :: Address
        offsetBits = instruction .&. 0x00FF
        signExtend x = if x `testBit` 7 then x .|. 0x7FFFFF00 else x

-- Parse an unconditional B instruction.
parseB2 :: RawInstruction -> Address -> Instruction
parseB2 instruction pc = B2 address
    where
        address = pc + offset
        offset = signExtend (fromIntegral offsetBits) `shiftL` 1 :: Address
        offsetBits = instruction .&. 0x07FF
        signExtend x = if x `testBit` 10 then x .|. 0x7FFFF800 else x


-- Printing stuff

-- Print an instruction using GAS syntax.
printInstruction :: Instruction -> String
printInstruction (B1 condition address) =
    printf "B%s %s" (show condition) (label address)
printInstruction (B2 address) =
    printf "B %s" (label address)
printInstruction (Unknown instruction) =
    printf ".halfword 0x%08X  @ unknown" instruction

printInstructions :: [Instruction] -> String
printInstructions = concatMap (printf "    %s\n" . printInstruction)

--Autogenerate a label based on an address.
label :: Address -> String
label = printf "thumb_0x%08X"


-- Miscellaneous

-- Return Just the target address of a branch instruction, or Nothing for a
-- non-branch instruction or a register branch.
branchAddress :: Instruction -> Maybe Address
branchAddress (B1 _ target) = Just target
branchAddress (B2 target) = Just target
branchAddress _ = Nothing


-- Disassembling

-- Get an instruction.
getInstruction :: Address -> Get Instruction
getInstruction sectionStart = parseInstruction <$> getWord16le <*> getPC
    where getPC = (2 + sectionStart +) . fromIntegral <$> bytesRead

-- Get a sequence of instructions.
disassembleSection :: Address -> Get [Instruction]
disassembleSection sectionStart = do
    instruction <- getInstruction sectionStart

    if stop instruction then
        return [instruction]
    else
        (instruction :) <$> disassembleSection sectionStart

-- Return True if disassembly should stop after this instruction, i.e. if the
-- instruction is an unconditional branch without link.
stop :: Instruction -> Bool
stop (B2 _) = True
stop _ = False
