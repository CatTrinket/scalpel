import Control.Monad ((>=>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Environment (getArgs)
import System.IO (Handle, IOMode(ReadMode), withBinaryFile)

import ARM (Address, Instruction)

type Disassembly = Map.Map Address [Instruction]

disassemble :: Disassembly -> Set.Set Address -> Handle -> IO Disassembly
disassemble = undefined

fullDisassembly :: Handle -> IO ()
fullDisassembly = disassemble Map.empty (Set.singleton 0) >=> printDisassembly

printDisassembly :: Disassembly -> IO ()
printDisassembly = undefined

main = do
    [filename] <- getArgs
    withBinaryFile filename ReadMode fullDisassembly
