module Language.RTL.Code
  ( verilog
  ) where

import Data.List
import Text.Printf

import Language.RTL.Core

verilog :: Name -> Module -> IO ()
verilog name m = writeFile (name ++ ".v") $ unlines
  [ "module " ++ name
  , "( input  reset"
  , ", input  clock"
  , unlines [ printf ", input  %-5s %s" (widthDecl w) name | (name, w) <- inputs m ] ++ unlines [ printf ", output %-5s %s" (widthDecl w) name | (name, a) <- outputs m, let w = width a ] ++ ");"
  , ""
  , unlines [ printf "reg  %-5s %s;" (widthDecl w) (pathName path) | (path, w) <- regs  m ]
  , unlines [ printf "wire %-5s %s;" (widthDecl w) (pathName path) | (path, w) <- wires m ]
  -- XXX
  , unlines [ printf "assign %s %s;" name (bv a) | (name, a) <- outputs m ]
  , "endmodule"
  ]

pathName :: Path -> String
pathName p = "\\" ++ intercalate "." p

widthDecl :: Int -> String
widthDecl w = if w == 1 then "" else "[" ++ show (w - 1) ++ ":0]"

bv :: BV -> String
bv a = case a of
  Input name _ -> name
  Reg   path _ -> pathName path
  Wire  path _ -> pathName path
  Const w a -> printf "%d'd%d" w a
  Concat a b
    | width a == 0 -> bv b
    | width b == 0 -> bv a
    | otherwise    -> "{" ++ bv a ++ ", " ++ bv b ++ "}"
  Select a msb lsb
    | msb == lsb -> msb - lsb + 1
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


