module Language.RTL.Core
  ( Name
  , Path
  , BV  (..)
  , Module (..)
  , Verification (..)
  , bitvectors
  , inputs
  , wires
  , regs
  , width
  ) where

import Data.Function
import Data.List

type Name = String
type Path = [Name]

-- | A bitvector expression.
data BV where
  Input  :: Name -> Int -> BV
  Reg    :: Path -> Int -> BV
  Wire   :: Path -> Int -> BV
  Const  :: Int -> Integer -> BV
  Concat :: BV -> BV -> BV
  Select :: BV -> Int -> Int -> BV
  Add    :: BV -> BV -> BV
  Sub    :: BV -> BV -> BV
  Mul    :: BV -> BV -> BV
  Not    :: BV -> BV
  And    :: BV -> BV -> BV
  Or     :: BV -> BV -> BV
  Xor    :: BV -> BV -> BV
  Eq     :: BV -> BV -> BV
  Lt     :: BV -> BV -> BV
  Gt     :: BV -> BV -> BV
  Le     :: BV -> BV -> BV
  Ge     :: BV -> BV -> BV
  Mux    :: BV -> BV -> BV -> BV
  deriving (Show, Eq)

data Verification
  = Assume Int Path BV
  | Assert Int Path Int BV

data Module = Module
  { assigns      :: [(BV, BV)]
  , outputs      :: [(Name, BV)]
  , verification :: [Verification]
  }

-- | All primary bitvector expressions in a module.
bitvectors :: Module -> [BV]
bitvectors m = nub $ (concat [ [a, b] | (a, b) <- assigns m ])
                  ++ [ case a of { Assume _ _ a -> a; Assert _ _ _ a -> a } | a <- verification m ]
                  ++ [ a | (_, a) <- outputs m ]

-- | All inputs in a module.
inputs :: Module -> [(Name, Int)]
inputs m = sortBy (compare `on` fst) [ (path, width) | Input path width <- bitvectors m ]

-- | All internal wires in a module.
wires :: Module -> [(Path, Int)]
wires m = sortBy (compare `on` fst) [ (path, width) | Wire path width <- bitvectors m ]

-- | All registers.
regs :: Module -> [(Path, Int)]
regs m = sortBy (compare `on` fst) [ (path, width) | Reg path width <- bitvectors m ]

-- | Width of bitvector.
width :: BV -> Int
width a = case a of
  Input _ a -> a
  Reg   _ a -> a
  Wire  _ a -> a
  Const a _ -> a
  Concat a b -> width a + width b
  Select _ msb lsb -> msb - lsb + 1
  Add    a _ -> width a
  Sub    a _ -> width a
  Mul    a _ -> width a
  Not    a -> width a
  And    a _ -> width a
  Or     a _ -> width a
  Xor    a _ -> width a
  Eq     _ _ -> 1
  Lt     _ _ -> 1
  Gt     _ _ -> 1
  Le     _ _ -> 1
  Ge     _ _ -> 1
  Mux _ a _ -> width a

