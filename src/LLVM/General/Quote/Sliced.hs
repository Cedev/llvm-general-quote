{-# LANGUAGE DeriveDataTypeable #-}

module LLVM.General.Quote.Sliced (
    -- * Sliced
    Sliced (..),
    -- * Constructors
    -- $constructors
    label,
    term,
    instr,
    instrs,
    block,
    blocks,
    -- * Inspection
    entireBlocks 
) where

import Data.Typeable

import Data.Monoid
import qualified LLVM.General.AST as L
  
type DiffList a = [a] -> [a]

-- | `Sliced` represents a segment of a list of 'BasicBlock's that might start in the middle of one block and end in the middle of another.
data Sliced
    -- | A list of 'Instruction's entirely contained within a single block.
    = Inside (DiffList (L.Named L.Instruction))
    -- | A list of `Instruction's that are around entire blocks
    | Around
        -- | The instructions and terminator that make up the end of a preceding block.
        -- These will be discarded if the preceding
        ([L.Named L.Instruction], L.Named L.Terminator)
        -- | Entire blocks
        (DiffList L.BasicBlock)
        -- | The label and instructions that make up the beginning of the next block
        (Maybe ([L.Named L.Instruction] -> L.Named L.Terminator -> L.BasicBlock))
      deriving (Typeable)
        
instance Monoid Sliced where
    mempty = Inside id
    Inside             f  `mappend` Inside g            = Inside (f . g)
    Inside             f  `mappend` Around (i, t) bs y  = Around (f i, t) bs y
    Around x bs  (Just f) `mappend` Inside g            = Around x        bs (Just (f . g))
    Around x bs  Nothing  `mappend` Inside _            = Around x        bs Nothing
    Around x bs1 (Just f) `mappend` Around (i, t) bs2 y = Around x (bs1 . (f i t:) . bs2) y
    Around x bs1 Nothing  `mappend` Around _      bs2 y = Around x (bs1 .            bs2) y

br :: L.Name -> L.Named L.Terminator
br name = L.Do $ L.Br name []
    
{- $constructors
These constructors provide easy ways to build 'Sliced' blocks
@
label name <> instrs instructions <> term terminator = block (BasicBlock name instructions terminator)
@
-}

{-|
Constructs the start of a 'BasicBlock' beginning with a label.
The preceding instructions unconditionally branch to the label.
'label name' is essentially
@
  br label %name
name:
@
-}
label :: L.Name -> Sliced
label name = Around ([], br name) id (Just (L.BasicBlock name))

-- | Constructs the end of a `BasicBlock` ending with a terminator. 
-- | Any instructions after 'term' are discarded.
-- | 'term t <> instrs i == term t'
term :: L.Named L.Terminator -> Sliced
term t = Around ([], t) id Nothing
    
-- | Constructs an instruction.
instr :: L.Named L.Instruction -> Sliced
instr i = Inside (i:)

-- | Constructs a list of instructions. 'instrs == foldr (\i sb -> instr i <> sb) mempty'
instrs :: [L.Named L.Instruction] -> Sliced
instrs is = Inside (is++)

{-|
Constructs an entire block.
The preceding instructions unconditionally branch to the block's label.
Any instructions after the block are discarded.
'block (BasicBlock name instructions terminator)' is essentiall
@
  br label %name
name:
  instrs
  terminator
@
-}
block :: L.BasicBlock -> Sliced
block b@(L.BasicBlock name _ _)    = Around ([], br name) (b:) Nothing

-- | Constructs a list of blocks. 'blocks == foldr (\i sb -> instr i <> sb) mempty'
blocks :: [L.BasicBlock] -> Sliced
blocks [] = mempty
blocks bs@(L.BasicBlock name _ _:_) = Around ([], br name) (bs++) Nothing

-- | Get all of the entirely defined blocks. Any preceding or trailing instructions are discarded.
-- | 'entireBlocks . blocks == id'
entireBlocks :: Sliced -> [L.BasicBlock]
entireBlocks (Inside _) = []
entireBlocks (Around _ bs _) = bs []