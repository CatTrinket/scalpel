module ARM (Address, Instruction, Register(..), Shift(..), ShifterOperand(..),
    Condition(..), branchAddress, disassembleSection, label, printInstructions)
    where

import Control.Applicative ((<$>), (<*>))
import Data.Binary.Get (Get, bytesRead, getWord32le)
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR, testBit)
import Data.Word (Word32)
import Text.Printf (printf)


-- Various data types

type Address = Word32
type RawInstruction = Word32

data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | SL | FP |
    IP | SP | LR | PC
    deriving (Show, Eq, Ord, Enum)

data ShifterOperand =
    Register Register |
    Immediate Int |
    ShiftImmediate Register Shift Int |
    ShiftRegister Register Shift Register
    deriving Show

data Shift = LSL | LSR | ASR | ROR
    deriving Show

data Condition = EQ | NE | CS | CC | MI | PL | VS | VC | HI | LS | GE | LT |
    GT | LE | AL | NV
    deriving (Show, Eq, Ord, Enum)

data Instruction =
    B Condition Address |
    BL Condition Address |
    BX Condition Register |
    Unknown RawInstruction
    deriving Show


-- Parsing instructions

-- Return True if the bitNum-th bit of x is 0.
testZeroBit :: Bits a => a -> Int -> Bool
x `testZeroBit` bitNum = not (x `testBit` bitNum)

-- Parse a raw 32-bit ARM instruction.
parseInstruction :: RawInstruction -> Address -> Instruction
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
parseBranch :: RawInstruction -> Condition -> Address -> Instruction
parseBranch instruction condition pc =
    if instruction `testZeroBit` 24 then
        B condition target
    else
        BL condition target
    where
        target = pc + offset
        offset = signExtend (instruction .&. 0x00FFFFFF) `shiftL` 2
        signExtend x = if x `testBit` 23 then x .|. 0x3F000000 else x


-- Printing stuff

-- Print an instruction using GAS syntax.
printInstruction :: Instruction -> String
printInstruction (B cond address) =
    printf "B%s %s" (showCond cond) (label address)
printInstruction (BL cond address) =
    printf "BL%s %s" (showCond cond) (label address)
printInstruction (BX cond register) =
    printf "BX%s %s" (showCond cond) (show register)
printInstruction (Unknown x) = printf ".word 0x%08X  @ unknown" x

-- Show a condition, unless it's AL.
showCond :: Condition -> String
showCond AL = ""
showCond c = show c

-- Print a sequence of instructions.
printInstructions :: [Instruction] -> String
printInstructions = concatMap (printf "    %s\n" . printInstruction)

-- Autogenerate a label based on an address.
label :: Address -> String
label = printf "arm_0x%08X"


-- Miscellaneous

-- Return Just the target address of a branch instruction, or Nothing for a
-- non-branch instruction or a register branch.
branchAddress :: Instruction -> Maybe Address
branchAddress (B _ target) = Just target
branchAddress (BL _ target) = Just target
branchAddress _ = Nothing

-- Turn a register number into the corresponding register.
bitsToRegister :: Integral a => a -> Register
bitsToRegister = toEnum . fromIntegral


-- Disassembling

-- Get an instruction.
getInstruction :: Address -> Get Instruction
getInstruction baseAddress = parseInstruction <$> getWord32le <*> getPC
    where getPC = (baseAddress +) . fromIntegral . (+ 4) <$> bytesRead

-- Get a sequence of instructions.
disassembleSection :: Address -> [Instruction] -> Get [Instruction]
disassembleSection baseAddress instructions = do
    instruction <- getInstruction baseAddress
    let instructions' = instruction:instructions

    if stop instruction then
        return (reverse instructions')
    else
        disassembleSection baseAddress instructions'

-- Return True if disassembly should stop after this instruction, i.e. if the
-- instruction is an unconditional branch without link.
stop :: Instruction -> Bool
stop (B AL _) = True
stop (BX AL _) = True
stop _ = False
