module ARM (Address, Instruction, Register(..), Operand2(..), Shift(..),
    Condition(..), disassemble, jumpAddress, label, printInstructions) where

import Control.Applicative ((<$>), (<*>))
import Data.Binary.Get (Get, bytesRead, getWord32le)
import Data.Bits (Bits, (.&.), shiftL, shiftR, testBit)
import Data.Word (Word32)
import Text.Printf (printf)

-- Various data types
data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | SL | FP |
    IP | SP | LR | PC
    deriving (Show, Eq, Ord, Enum)

data Operand2 =
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

type Address = Integer

type RawInstruction = Word32

data Instruction =
    B Condition Address |
    BL Condition Address |
    BX Condition Register |
    Unknown RawInstruction
    deriving Show

getInstruction :: Address -> Get Instruction
getInstruction baseAddress = parseInstruction <$> getWord32le <*> getAddress
    where getAddress = (baseAddress +) . fromIntegral <$> bytesRead

-- Parsing instructions
testZeroBit :: Bits a => a -> Int -> Bool
x `testZeroBit` bitNum = not (x `testBit` bitNum)

parseInstruction :: RawInstruction -> Address -> Instruction
parseInstruction instruction position =
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
                    parseBranch instruction cond position
            else
                Unknown instruction
    else
        Unknown instruction
    where cond = toEnum . fromIntegral $ instruction `shiftR` 28 :: Condition

parseBranch :: RawInstruction -> Condition -> Address -> Instruction
parseBranch instruction condition address =
    if target < 0 then
        -- welp
        Unknown instruction
    else if instruction `testZeroBit` 24 then
        B condition target
    else
        BL condition target
    where
        target = address + 4 + offset
        offset = fromIntegral offset' - fromIntegral signBit :: Integer
        offset' = ((instruction .&. 0x007FFFFF) `shiftL` 2)
        signBit = (instruction .&. 0x00800000) `shiftL` 2

-- Printing shit
printInstruction :: Instruction -> String
printInstruction (B cond address) =
    printf "B%s %s" (showCond cond) (label address)
printInstruction (BL cond address) =
    printf "BL%s %s" (showCond cond) (label address)
printInstruction (BX cond register) =
    printf "BX%s %s" (showCond cond) (show register)
printInstruction (Unknown x) = printf ".word 0x%08X  @ unknown" x

showCond :: Condition -> String
showCond AL = ""
showCond c = show c

printInstructions :: [Instruction] -> String
printInstructions = concatMap (printf "    %s\n" . printInstruction)

label :: Address -> String
label = printf "arm_0x%08X"

-- Stuff
jumpAddress :: Instruction -> Maybe Address
jumpAddress (B _ target) = Just target
jumpAddress (BL _ target) = Just target
jumpAddress _ = Nothing

bitsToRegister :: Integral a => a -> Register
bitsToRegister = toEnum . fromIntegral

-- Disassembling
disassemble :: Address -> [Instruction] -> Get [Instruction]
disassemble baseAddress instructions = do
    instruction <- getInstruction baseAddress
    let instructions' = instruction:instructions

    if stop instruction then
        return (reverse instructions')
    else
        disassemble baseAddress instructions'

stop :: Instruction -> Bool
stop (B AL _) = True
stop (BX AL _) = True
stop _ = False
