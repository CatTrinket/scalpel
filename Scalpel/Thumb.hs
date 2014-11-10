module Scalpel.Thumb (ThumbInstruction, thumbLabel) where

import Control.Applicative ((<$>), (<*>))
import Data.Binary.Get (Get, bytesRead, getWord16le)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit)
import Data.Word (Word16)
import Text.Printf (printf)

import Scalpel (Address, Condition(..), Instruction(..))


type RawInstruction = Word16

data ThumbInstruction =
    B1 Condition Address |
    B2 Address |
    Unknown RawInstruction
    deriving Show

instance Instruction ThumbInstruction where
    getInstruction sectionStart = parseInstruction <$> getWord16le <*> getPC
        where getPC = (2 + sectionStart +) . fromIntegral <$> bytesRead

    branchAddress (B1 _ target) = Just target
    branchAddress (B2 target) = Just target
    branchAddress _ = Nothing

    stop (B2 _) = True
    stop _ = False

    showGAS (B1 condition address) =
        printf "B%s %s" (show condition) (thumbLabel address)
    showGAS (B2 address) =
        printf "B %s" (thumbLabel address)
    showGAS (Unknown instruction) =
        printf ".halfword 0x%08X  @ unknown" instruction

-- Parse a raw 16-bit Thumb instruction.
parseInstruction :: RawInstruction -> Address -> ThumbInstruction
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
parseB1 :: RawInstruction -> Address -> ThumbInstruction
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
parseB2 :: RawInstruction -> Address -> ThumbInstruction
parseB2 instruction pc = B2 address
    where
        address = pc + offset
        offset = signExtend (fromIntegral offsetBits) `shiftL` 1 :: Address
        offsetBits = instruction .&. 0x07FF
        signExtend x = if x `testBit` 10 then x .|. 0x7FFFF800 else x

-- Autogenerate a label based on an address.
thumbLabel :: Address -> String
thumbLabel = printf "thumb_0x%08X"
