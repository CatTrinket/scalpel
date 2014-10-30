module ARM.Common where

import Data.Bits (Bits, testBit)
import Data.Word (Word32)


type Address = Word32

data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | SL | FP |
    IP | SP | LR | PC
    deriving (Show, Eq, Ord, Enum)

data Condition = EQ | NE | CS | CC | MI | PL | VS | VC | HI | LS | GE | LT |
    GT | LE | AL | NV
    deriving (Show, Eq, Ord, Enum)


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
