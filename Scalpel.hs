module Scalpel where

import Control.Applicative ((<$>))
import Data.Binary (Get)
import Data.Bits (Bits, testBit)
import Data.Word (Word32)


-- Type stuff

class Instruction a where
    stop :: a -> Bool
    branchAddress :: a -> Maybe Address
    printInstruction :: a -> String

type Address = Word32

data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | SL | FP |
    IP | SP | LR | PC
    deriving (Show, Eq, Ord, Enum)

data Condition = EQ | NE | CS | CC | MI | PL | VS | VC | HI | LS | GE | LT |
    GT | LE | AL | NV
    deriving (Show, Eq, Ord, Enum)


-- Get stuff
getInstruction :: Instruction a => Address -> Get a
getInstruction = undefined

getSection :: Instruction a => Address -> Get [a]
getSection sectionStart = do
    instruction <- getInstruction sectionStart

    if stop instruction then
        return [instruction]
    else
        (instruction :) <$> getSection sectionStart


-- Functions

-- Return True if the bitNum-th bit of x is 0.
testZeroBit :: Bits a => a -> Int -> Bool
x `testZeroBit` bitNum = not (x `testBit` bitNum)

-- Turn a register number into the corresponding register.
bitsToRegister :: Integral a => a -> Register
bitsToRegister = toEnum . fromIntegral

-- Show a condition, unless it's AL.
showCondition :: Condition -> String
showCondition AL = ""
showCondition c = show c
