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
    labelUnlabeled,
    -- * Inspection
    entireBlocks,
    isUnlabeled,
    isUnterminated,
    startLabel
) where

import Data.Typeable
import Data.Data

import Data.Monoid
import qualified LLVM.General.AST as L
  
-- | `Sliced` represents a segment of a list of 'BasicBlock's that might start in the middle of one block and end in the middle of another.
data Sliced = Slice
        -- | Instructions and that make up the end of a preceding block.
        -- | These instructions will be discarded if the preceding slices don't start the block.
        [L.Named L.Instruction]
        -- | Terminator for the end of a preceding block.
        -- | If the terminator is 'Nothing' flow continues through the slice.
        -- | The terminator will be discarded if the preceding slices don't start the block.
        (Maybe (L.Named L.Terminator))
        -- | Entire blocks
        [L.BasicBlock]
        -- | The label and instructions that make up the beginning of the next block
        (Maybe (L.Name, [L.Named L.Instruction]))
      deriving (Eq, Read, Show, Typeable, Data)
        
instance Monoid Sliced where
    mempty  = Slice [] Nothing [] Nothing
    mappend = combine  

combine :: Sliced -> Sliced -> Sliced
combine (Slice w Nothing []  Nothing) (Slice y t2 bs2 l2) = Slice (w ++ y) t2  bs2         l2
combine (Slice w t1      bs1 Nothing) (Slice _ _  bs2 l2) = Slice w        t1 (bs1 ++ bs2) l2
combine (Slice w t1 bs1 (Just (n1, x))) s2 =
    case s2 of
        (Slice y Nothing  [] Nothing       )              -> Slice w t1  bs1           (Just (n1, x ++ y))
        (Slice y Nothing  [] (Just (n2, z)))              -> Slice w t1 (bs1 ++ [L.BasicBlock n1 (x ++ y) (br n2)]) (Just (n2, z))
        (Slice y Nothing  bs2@(L.BasicBlock bn _ _:_) l2) -> Slice w t1 (bs1 ++ [L.BasicBlock n1 (x ++ y) (br bn)] ++ bs2) l2
        (Slice y (Just t2) bs2                        l2) -> Slice w t1 (bs1 ++ [L.BasicBlock n1 (x ++ y) t2     ] ++ bs2) l2
    
br :: L.Name -> L.Named L.Terminator
br name = L.Do $ L.Br name []
    
{- $constructors
These constructors provide easy ways to build 'Sliced' blocks
@
label name <> instrs instructions <> term terminator == block (BasicBlock name instructions terminator)
@
-}

{-|
Constructs the start of a 'BasicBlock' beginning with a label.
Any preceding unterminated instructions unconditionally branch to the label.
'label name' is essentially
@
  br label %name
name:
@
-}
label :: L.Name -> Sliced
label name = Slice [] Nothing [] (Just (name, []))

-- | Constructs the end of a `BasicBlock` ending with a terminator. 
-- | Any instructions after 'term' are discarded.
-- | 'term t <> instrs i == term t'
-- | 'term t1 <> term t2 == term t1'
term :: L.Named L.Terminator -> Sliced
term t = Slice [] (Just t) [] Nothing
    
-- | Constructs an instruction.
instr :: L.Named L.Instruction -> Sliced
instr i = Slice [i] Nothing [] Nothing

-- | Constructs a list of instructions. 'instrs == foldr (\i sb -> instr i <> sb) mempty'
instrs :: [L.Named L.Instruction] -> Sliced
instrs i = Slice i Nothing [] Nothing

{-|
Constructs an entire block.
The preceding instructions unconditionally branch to the block's label.
Any instructions after the block are discarded.
'block (BasicBlock name instructions terminator)' is essentially
@
  br label %name
name:
  instrs
  terminator
@
-}
block :: L.BasicBlock -> Sliced
block b = Slice [] Nothing [b] Nothing

-- | Constructs a list of blocks. 'blocks == foldr (\i sb -> instr i <> sb) mempty'
blocks :: [L.BasicBlock] -> Sliced
blocks bs = Slice [] Nothing bs Nothing

-- | Adds a label to the start of a slice if it doesn't already have a label at the start.
labelUnlabeled :: L.Name -> Sliced -> (L.Name, Sliced)
labelUnlabeled name s =
    case startLabel s of
        Just start -> (start, s)
        _          -> (name, label name <> s)

-- | Gets all of the entirely defined blocks. Any preceding or trailing instructions are discarded.
-- | 'entireBlocks . blocks == id'
entireBlocks :: Sliced -> [L.BasicBlock]
entireBlocks (Slice _ _ bs _) = bs

-- | `True` when the slice has unlabeled instructions or a terminator that will be discarded 
-- | if the preceding slices don't start the block.
isUnlabeled :: Sliced -> Bool
isUnlabeled (Slice [] Nothing _ _) = False
isUnlabeled (Slice _  _       _ _) = True

-- | `True` when the slice has a label that starts an unterminated block.
isUnterminated :: Sliced -> Bool
isUnterminated (Slice _ _ _ Nothing) = False
isUnterminated (Slice _ _ _ _      ) = True

-- | Gets the label that precedes everything else in the slice, if it exists.
startLabel :: Sliced -> Maybe L.Name
startLabel (Slice [] Nothing [] (Just (name, _))) = Just name
startLabel (Slice [] Nothing (L.BasicBlock name _ _:_) _) = Just name
startLabel _ = Nothing
