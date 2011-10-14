module Language.RTL
  (
  -- * Types
    BV
  , StorageType
  , RTL
  -- * Expressions
  -- ** Constants
  , true
  , false
  , constant
  -- ** Bit selection and concatenation.
  , (!)
  , (+++)
  -- ** Logical Operations
  , and_
  , or_
  , any_
  , all_
  , (-->)
  -- ** Equality and Comparison
  , (==.)
  , (/=.)
  , (<.)
  , (<=.)
  , (>.)
  , (>=.)
  -- ** Arithmetic Operations
  , (*.)
  -- ** Conditional Operator
  , mux
  -- * Statements
  -- ** Labels
  , (-|)
  -- ** Bitvector Variable Declarations
  , input
  , output
  , local
  , global
  , wire
  , reg
  -- ** Register and wire assignment.
  , (<==)
  -- ** Assertions.
  , assert
  , assume
  -- * Verification
  -- , verify
  -- * Code Generation
  , code
  , verilog
  ) where

import Control.Monad
import Data.Bits
import Data.Function
import Data.List

import Language.RTL.Core
import Language.RTL.Code

infixl 9 !
infixl 8 +++
infixl 7 *.
infix  4 ==., /=., <., <=., >., >=.
infixr 1 -->
infixr 0 <==, -|

invalidWidth :: String -> BV -> a
invalidWidth op bv = error $ "Incompatiable bitvector widths (" ++ op ++ "): " ++ show bv

instance Num BV where
  a + b
    | width a == width b && width a > 0 = Add a b
    | otherwise = invalidWidth "+" $ Add a b
  a - b
    | width a == width b && width a > 0 = Sub a b
    | otherwise = invalidWidth "-" $ Sub a b
  a * b
    | width a == width b && width a > 0 = Mul a b
    | otherwise = invalidWidth "*" $ Mul a b
  negate a = Const (width a) 0 - a
  abs a = a
  signum = undefined
  fromInteger = undefined

instance Bits BV where
  a .&. b
    | width a == width b && width a > 0 = And a b
    | otherwise = invalidWidth ".&." $ And a b
  a .|. b
    | width a == width b && width a > 0 = Or a b
    | otherwise = invalidWidth ".|." $ Or a b
  a `xor` b
    | width a == width b && width a > 0 = Xor a b
    | otherwise = invalidWidth ".|." $ Xor a b
  complement a
    | width a > 0 = Not a
    | otherwise = invalidWidth "complement" $ Not a
  shift = error "shift undefined"
  rotate = error "rotate undefined"
  bit = error "bit undefined"
  setBit a n
    | n < width a && n >= 0 = a .|. bit' (width a) n
    | otherwise = invalidWidth "setBit" a
  clearBit a n
    | n < width a && n >= 0 = a .&. complement (bit' (width a) n)
    | otherwise = invalidWidth "clearBit" a
  bitSize = width
  isSigned _ = False

bit' :: Int -> Int -> BV
bit' w n = Const (max 0 $ w - n - 1) 0 +++ true +++ Const (max 0 n) 0

class BitSelect a where (!) :: BV -> a -> BV

instance BitSelect Int where
  a ! n
    | n < width a && n >= 0 = Select a n n
    | otherwise = invalidWidth "!" $ Select a n n

instance BitSelect [Int] where
  a ! ns = foldl1 (+++) [ a ! n | n <- ns ]

instance BitSelect (Int, Int) where
  a ! (msb, lsb)
    | msb < width a && msb >= 0 && lsb < width a && lsb >= 0 && msb >= lsb = Select a msb lsb
    | lsb > msb = Const 0 0
    | otherwise = invalidWidth "!" $ Select a msb lsb

(+++) :: BV -> BV -> BV
a +++ b
  | width a == 0 = b
  | width b == 0 = a
  | otherwise = Concat a b


-- | True term.
true :: BV
true = Const 1 1

-- | False term.
false :: BV
false = Const 1 0

-- | Arbitrary constants.
constant :: Int -> Integer -> BV
constant = Const

-- | The conjunction of a BV list.
and_ :: [BV] -> BV
and_ a
  | all (\ b -> width b == width (head a)) a = foldl1 (.&.) a
  | otherwise = error $ "widths don't match (and_): " ++ show a

-- | The disjunction of a BV list.
or_ :: [BV] -> BV
or_ a
  | all (\ b -> width b == width (head a)) a = foldl1 (.|.) a
  | otherwise = error $ "widths don't match (or_): " ++ show a

-- | True iff the predicate is true for all elements.
all_ :: (a -> BV) -> [a] -> BV
all_ f a = and_ $ map f a

-- | True iff the predicate is true for any element.
any_ :: (a -> BV) -> [a] -> BV
any_ f a = or_ $ map f a

-- | Logical implication.
(-->) :: BV -> BV -> BV
a --> b = complement a .|. b

-- | Equal.
(==.) :: BV -> BV -> BV
a ==. b
  | width a == width b && width a > 0 = Eq a b
  | otherwise = invalidWidth "==." $ Eq a b

-- | Not equal.
(/=.) :: BV -> BV -> BV
a /=. b = complement $ a ==. b

-- | Less than.
(<.) :: BV -> BV -> BV
a <. b
  | width a == width b && width a > 0 = Lt a b
  | otherwise = invalidWidth "<." $ Lt a b

-- | Greater than.
(>.) :: BV -> BV -> BV
a >. b
  | width a == width b && width a > 0 = Gt a b
  | otherwise = invalidWidth ">." $ Gt a b

-- | Less than or equal.
(<=.) :: BV -> BV -> BV
a <=. b
  | width a == width b && width a > 0 = Le a b
  | otherwise = invalidWidth "<=." $ Le a b

-- | Greater than or equal.
(>=.) :: BV -> BV -> BV
a >=. b
  | width a == width b && width a > 0 = Ge a b
  | otherwise = invalidWidth ">=." $ Ge a b

{-
-- | Returns the minimum of two numbers.
min_ :: NumE a => E a -> E a -> E a
min_ a b = mux (a <=. b) a b

-- | Returns the minimum of a list of numbers.
minimum_ :: NumE a => [E a] -> E a
minimum_ = foldl1 min_

-- | Returns the maximum of two numbers.
max_ :: NumE a => E a -> E a -> E a
max_ a b = mux (a >=. b) a b

-- | Returns the maximum of a list of numbers.
maximum_ :: NumE a => [E a] -> E a
maximum_ = foldl1 max_

-- | Limits between min and max.
limit :: NumE a => E a -> E a -> E a -> E a
limit a b i = max_ min $ min_ max i
  where
  min = min_ a b
  max = max_ a b
-}

-- | Multiplication.
(*.) :: BV -> BV -> BV
a *. b
  | width a == width b && width a >= 1 = Mul a b
  | otherwise = invalidWidth "*" $ Mul a b

-- | Conditional expression.
--
-- > mux test onTrue onFalse
mux :: BV -> BV -> BV -> BV
mux a b c
  | width a == 1 && width b == width c && width b > 0 = Mux a b c
  | otherwise = invalidWidth "mux" $ Mux a b c

-- | Labels a statement and creates a new variable scope.
--   Labels are used in counter examples to help trace the program execution.
(-|) :: Name -> RTL a -> RTL a
name -| (RTL f1) = RTL f2
  where
  f2 (id1, path, m1) = (a, (id2, path, m2))
    where
    (a, (id2, _, m2)) = f1 (id1, path ++ [name], m1)

{-
-- | Generic variable declaration.
var :: AllE a => Name -> a -> Stmt (V a)
var name init = do
  path <- getPath
  return $ V False (path ++ [name]) init

-- | Generic variable declaration and immediate assignment.
var' :: AllE a => Name -> E a -> Stmt (E a)
var' name value = do
  a <- var name zero
  a <== value
  return $ ref a
-}

-- | Input declaration.
input  :: Name -> Int -> BV
input name width
  | width > 0 = Input name width
  | otherwise = invalidWidth "input" $ Input name width

-- | Output declaration.
output :: Name -> BV -> RTL ()
output name a = RTL $ \ (id, path, m) -> ((), (id, path, m { outputs = outputs m ++ [(name, a)] }))

type StorageType = Path -> Int -> BV

wire :: StorageType
wire = Wire

reg :: StorageType
reg = Reg

-- | Global wire or reg declaration.
global :: StorageType -> Path -> Int -> BV
global a path w
  | w > 0 = a path w
  | otherwise = invalidWidth "global" $ a path w

-- | Local wire or reg declaration.
local :: StorageType -> Name -> Int -> RTL BV
local a name w
  | w > 0     = RTL $ \ (id, path, m) -> (a (path ++ [name]) w, (id, path, m))
  | otherwise = RTL $ \ (_, path, _) -> invalidWidth "local" $ a (path ++ [name]) w

-- | The RTL monad holds variable declarations and assignments.
data RTL a = RTL ((Int, [Name], Module) -> (a, (Int, [Name], Module)))

instance Monad RTL where
  return a = RTL $ \ s -> (a, s)
  (RTL f1) >>= f2 = RTL f3
    where
    f3 s1 = f4 s2
      where
      (a, s2) = f1 s1
      RTL f4 = f2 a

evalRTL :: RTL () -> Module
evalRTL (RTL f) = m { outputs = sortBy (compare `on` fst) $ outputs m }
  where
  ((), (_, _, m)) = f (0, [], Module [] [] [])

--class Assign a where (<==) :: V a -> E a -> Stmt ()
--instance AllE a => Assign a where a <== b = statement $ Assign a b
(<==) :: BV -> BV -> RTL ()
a <== b
  | width a == width b && width a > 0 = RTL $ \ (id, path, m) -> ((), (id, path, m { assigns = assigns m ++ [(a, b)] }))  --XXX Check for valid width and and lhs.
  | otherwise = invalidWidth "<==" $ Eq a b  --XXX Find replacement to Eq.

-- | Assume a condition is true.
--   Assumptions are used as lemmas to other assertions.
assume :: Name -> BV -> RTL ()
assume name a
  | width a == 1 = RTL $ \ (id, path, m) -> ((), (id + 1, path, m { verification = verification m ++ [Assume id (path ++ [name]) a] }))
  | otherwise = invalidWidth "assume" a

-- | Defines a new assertion.
--
-- > assert name k proposition
assert :: Name -> Int -> BV -> RTL ()
assert name k a
  | k < 1 = error $ "k-induction search depth must be > 0: " ++ name ++ " k = " ++ show k
  | width a == 1 = RTL $ \ (id, path, m) -> ((), (id + 1, path, m { verification = verification m ++ [Assert id (path ++ [name]) k a] }))
  | otherwise = invalidWidth "assert" a

-- | Code generation.
code :: (Name -> Module -> IO ()) -> Name -> RTL () -> IO ()
code f name rtl = f name $ evalRTL rtl

{-
-- | Verify a program.
--
-- > verify pathToYices program
verify :: FilePath -> Stmt () -> IO ()
verify yices program = analyze (V.verify yices) program
-}

