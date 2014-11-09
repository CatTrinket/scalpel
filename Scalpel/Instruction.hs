module Scalpel.Instruction (Instruction(..), getSection) where

import Control.Applicative ((<$>))
import Data.Binary (Get)

import ARM.Common (Address)


class Instruction a where
    stop :: a -> Bool
    branchAddress :: a -> Maybe Address
    printInstruction :: a -> String

getInstruction :: Instruction a => Address -> Get a
getInstruction = undefined

getSection :: Instruction a => Address -> Get [a]
getSection sectionStart = do
    instruction <- getInstruction sectionStart

    if stop instruction then
        return [instruction]
    else
        (instruction :) <$> getSection sectionStart
