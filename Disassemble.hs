import Control.Applicative ((<$>))
import Control.Monad ((>=>))
import Data.Binary.Get (runGet)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Lazy as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import System.Environment (getArgs)
import System.IO (Handle, IOMode(ReadMode), SeekMode(AbsoluteSeek), hSeek,
    hTell, withBinaryFile)
import Text.Printf (printf)

import ARM (Address, Instruction, disassembleSection, jumpAddress, label,
    printInstructions)

type Disassembly = Map.Map Address (Maybe [Instruction])

disassemble :: Disassembly -> Set.Set Address -> Handle -> IO Disassembly
disassemble past future binary = do
    let (address, future') = Set.deleteFindMin future
    hSeek binary AbsoluteSeek (fromIntegral address)

    section <- runGet (disassembleSection address []) <$>
        BL.hGetContents binary

    let past' = Map.insert address (Just section) past
    (past'', future'') <- updateAddresses past' future' section address <$>
        fromIntegral <$> hTell binary

    if Set.null future'' then
        return past''
    else
        disassemble past'' future'' binary

updateAddresses :: Disassembly -> Set.Set Address -> [Instruction] ->
    Address -> Address -> (Disassembly, Set.Set Address)
updateAddresses past future section start end = (newPast, newFuture)
    where
        newAddresses = Set.fromList (mapMaybe jumpAddress section)
        isInside address = start <= address && address < end
        (inside, outside) = Set.partition isInside newAddresses
        newPast = past `Map.union` Map.fromSet (const Nothing) inside
        newFuture = future `Set.union` outside `Set.difference` Map.keysSet past

fullDisassembly :: Handle -> IO ()
fullDisassembly = disassemble Map.empty (Set.singleton 0) >=> printDisassembly

printDisassembly :: Disassembly -> IO ()
printDisassembly = mapM_ printSection . Map.assocs

printSection :: (Address, Maybe [Instruction]) -> IO ()
printSection (_, Nothing) = return ()
printSection (address, Just section) = do
    printf "%s:\n" (label address) :: IO ()
    putStrLn (printInstructions section)

main = do
    [filename] <- getArgs
    withBinaryFile filename ReadMode fullDisassembly
