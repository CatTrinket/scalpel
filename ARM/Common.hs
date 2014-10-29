module ARM.Common (Address, Register(..), bitsToRegister) where

import Data.Word (Word32)


type Address = Word32

data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | SL | FP |
    IP | SP | LR | PC
    deriving (Show, Eq, Ord, Enum)

-- Turn a register number into the corresponding register.
bitsToRegister :: Integral a => a -> Register
bitsToRegister = toEnum . fromIntegral
