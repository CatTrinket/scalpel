import Control.Applicative ((<$>))
import Control.Monad ((>=>))
import Data.Binary.Get (runGet)
import qualified Data.ByteString.Lazy as BL
import Data.List (isSuffixOf)
import qualified Data.Map.Lazy as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import System.Environment (getArgs)
import System.IO (Handle, IOMode(ReadMode), SeekMode(AbsoluteSeek), hSeek,
    hTell, withBinaryFile)
import Text.Printf (printf)

import ARM.Common (Address)
import qualified ARM.ARM as ARM
import qualified ARM.Thumb as Thumb


-- A map from section addresses to sections.
type Disassembly = Map.Map Address Section

-- A section of ARM or Thumb code, or a "subsection", i.e. a branch target in
-- the middle of another section.
data Section =
    ARMSection [ARM.Instruction] |
    ThumbSection [Thumb.Instruction] |
    Subsection

-- A set of section addresses yet to be disassembled.
type Future = Set.Set (Address, Mode)

-- An instruction set, either ARM or Thumb.
-- XXX "Mode" might not be a particularly good name for this.
data Mode = ARMMode | ThumbMode deriving (Eq, Ord)

-- Continue disassembling from a disassembly in progress and a set of sections
-- to disassemble.
disassemble :: Disassembly -> Future -> Handle -> IO Disassembly
disassemble past future binary = do
    let ((address, mode), future') = Set.deleteFindMin future
    hSeek binary AbsoluteSeek (fromIntegral address)

    section <- disassembleSection mode address <$> BL.hGetContents binary

    let past' = Map.insert address section past
    (past'', future'') <- updateAddresses past' future' section mode address .
        fromIntegral <$> hTell binary

    if Set.null future'' then
        return past''
    else
        disassemble past'' future'' binary

-- Disassemble a section, be it ARM or Thumb.
disassembleSection :: Mode -> Address -> BL.ByteString -> Section
disassembleSection ARMMode address =
    ARMSection . runGet (ARM.disassembleSection address)
disassembleSection ThumbMode address =
    ThumbSection . runGet (Thumb.disassembleSection address)

-- Update a disassembly in progress and a set of addresses to look at, based on
-- all the branch addresses in a section.
updateAddresses :: Disassembly -> Future -> Section -> Mode -> Address ->
    Address -> (Disassembly, Future)
updateAddresses past future section mode start end = (newPast, newFuture)
    where
        isInside address = start <= address && address < end
        (inside, outside) = Set.partition isInside (branchAddresses section)
        newPast = past `Map.union` Map.fromSet (const Subsection) inside
        newFuture = future `Set.union` Set.map withMode outside
           `Set.difference` Set.map withMode (Map.keysSet past)
        withMode x = (x, mode)

-- Extract the target addresses of all the branch instructions in a section.
branchAddresses :: Section -> Set.Set Address
branchAddresses (ARMSection section) =
    Set.fromList (mapMaybe ARM.branchAddress section)
branchAddresses (ThumbSection section) =
    Set.fromList (mapMaybe Thumb.branchAddress section)
branchAddresses Subsection = undefined

-- Disassemble as much of a binary as possible.
fullDisassembly :: Handle -> IO Disassembly
fullDisassembly = disassemble Map.empty (Set.singleton (0, ARMMode))

-- Print a disassembly.
printDisassembly :: Disassembly -> IO ()
printDisassembly = mapM_ printSection . Map.assocs

-- Print a section of a disassembly.
printSection :: (Address, Section) -> IO ()
printSection (_, Subsection) = return ()
printSection (address, section) = do
    printf "%s:\n" (label section address) :: IO ()
    putStrLn (printInstructions section)
    where
        label (ARMSection _) = ARM.label
        label (ThumbSection _) = Thumb.label
        printInstructions (ARMSection sec) = ARM.printInstructions sec
        printInstructions (ThumbSection sec) = Thumb.printInstructions sec

-- Disassemble as much of Pokémon FireRed Version as possible.  This function
-- is VERY TEMPORARY.
disassembleFireRed :: Handle -> IO Disassembly
disassembleFireRed = disassemble Map.empty
    (Set.fromList [(0, ARMMode), (0x3A4, ThumbMode)])

-- Disassemble a binary from the command line.
main = do
    [filename] <- getArgs

    -- XXX I know I should be using some filepath function but whatever
    let isFireRed = "firered.gba" `isSuffixOf` filename
    let disFunc = if isFireRed then disassembleFireRed else fullDisassembly

    withBinaryFile filename ReadMode (disFunc >=> printDisassembly)
