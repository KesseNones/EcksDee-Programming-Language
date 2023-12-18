--Jesse A. Jones
--Version: 2023-12-18.93
--Toy Programming Language Named EcksDee

{-
    ISSUES:
        -Extra error checking for casting is a good idea.
        -Maybe have errors show line number 
            of code file where error happened, somehow. 
            It would make user debugging much less ass.
        -Standardize errors.
-}

import Data.List
import Data.Char
import Data.Maybe 
import Debug.Trace
import Text.Read (readMaybe)
import System.IO
import System.Environment
import qualified Data.Map.Strict as M

data Value =                                         
        BigInteger Integer
    |   Integer Int
    |   Float Float
    |   Double Double
    |   String {chrs :: [Char], len :: Int}
    |   Char Char
    |   Boolean Bool
    |   List { items :: [Value], len :: Int}
    deriving (Eq, Show, Ord)

-- or it can be an operation, which has a string name.
-- the program "2 2 +" would have tokens [ I 2, I 2, Op "+" ] 
data Token = 
        Val Value 
    |   Word String
    deriving ( Eq, Show )
-- Deriving "Eq" means that we can use == and /= between tokens. E.g., I 2 == I 2
-- Deriving "Show" means that we can use "print" on tokens. 

-- An abstract syntax tree
data AstNode =
        -- a single token 
        Terminal Token 

        -- an if node. contains two branches: one for true and one for false. 
    |   If { ifTrue :: AstNode, ifFalse :: AstNode }

        -- a while node. contains only a child node for the body of the loop. 
    |   While AstNode

        -- a list of nodes. Represents a sequence of instructions like "1 1 + 2 *"
    |   Expression [ AstNode ]

    |   Function {funcCmd :: AstNode, funcName :: AstNode, funcBod :: AstNode}

    |   Variable {varName :: AstNode, varCmd :: AstNode}

    deriving ( Show )

-- This is the state of the interpreter. 
-- Currently it stores the stack, which is where all of the data lives. 
data EDState = EDState { 
    stack :: [Value], 
    fns :: M.Map String AstNode,
    vars :: M.Map String Value
}

--Adds two values together. If the types can't be added, throw an error.
--Can also act as an OR operator for Boolean type.
addVals :: Value -> Value -> Value
addVals (BigInteger a) (BigInteger b) = BigInteger (a + b)
addVals (Integer a) (Integer b) = Integer (a + b)
addVals (Double a) (Double b) = Double (a + b)
addVals (Float a) (Float b) = Float (a + b)
addVals (Boolean a) (Boolean b) = Boolean (a || b)
addVals _ _ = error "Operator (+) error. \n Can't add types together that aren't BigIntegers, Integers, Floats, or Booleans. \n Data types also need to match."

-- Subtracts two values. If the types can't be subtracted, throw an error.
subVals :: Value -> Value -> Value
subVals (BigInteger a) (BigInteger b) = BigInteger (b - a)
subVals (Integer a) (Integer b) = Integer (b - a)
subVals (Double a) (Double b) = Double (b - a)
subVals (Float a) (Float b) = Float (b - a)
subVals _ _ = error "Operator (-) error. \n Can't subtract types that aren't BigIntegers, Integers, or Floats. \n Data types also need to match."

--Multiplies two values together. If the types can't be multiplied, throw an error.
multVals :: Value -> Value -> Value
multVals (BigInteger a) (BigInteger b) = BigInteger (a * b)
multVals (Integer a) (Integer b) = Integer (a * b)
multVals (Double a) (Double b) = Double (a * b)
multVals (Float a) (Float b) = Float (a * b)
multVals (Boolean a) (Boolean b) = Boolean (a && b)
multVals _ _ = error "Operator (*) error. \n Can't multiply types together that aren't BigIntegers, Integers, Floats, Doubles, or Booleans. \n Data types also need to match."

-- Divides two values. If the types can't be divided, throw an error.
divideVals :: Value -> Value -> Value
divideVals (BigInteger a) (BigInteger b) = BigInteger (b `div` a)
divideVals (Integer a) (Integer b) = Integer (b `div` a)
divideVals (Double a) (Double b) = Double (b / a)
divideVals (Float a) (Float b) = Float (b / a)
divideVals _ _ = error "Operator (/) error. \n Can't divide types that aren't BigIntegers, Integers, or Floats. \n Data types also need to match"

modVals :: Value -> Value -> Value
modVals (BigInteger a) (BigInteger b) = BigInteger (b `mod` a)
modVals (Integer a) (Integer b) = Integer (b `mod` a)
modVals _ _ = error "Operator (%) error. \n Can't perform modulo on types that aren't BigIntegers or Integers. \n Data types also have to match."

--Concatenates two Strings or Lists.
doConcat' :: Value -> Value -> Value
doConcat' (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = String {chrs = acs ++ bcs, len = al + bl}
doConcat' (List {items = as, len = al}) (List {items = bs, len = bl}) = List {items = as ++ bs, len = al + bl}
doConcat' _ _ = error "Operator (++) error. \n Can't perform concatenation on types that aren't Strings or Lists."

--Concatentates two Strings/Lists together.
doConcat :: EDState -> IO EDState
doConcat state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (++) error. Concatenation requires two operands!"
        [x] -> error "Operator (++) error. Concatenation requires two operands!"
        vals -> do 
            let (state', a, b) = fsPop2 state
            return (fsPush (doConcat' a b) state')

--Adds two values on stack if they can be added.
doAdd :: EDState -> IO EDState
doAdd state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (+) error. Addition requires two operands!"
        [x] -> error "Operator (+) error. Addition requires two operands!"
        vals -> do 
            let (state', a, b) = fsPop2 state
            return (fsPush (addVals a b) state')

--Subtracts two values on stack if they can be subtracted.
doSub :: EDState -> IO EDState
doSub state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (-) error. Subtraction requires two operands!"
        [x] -> error "Operator (-) error. Subtraction requires two operands!"
        vals -> do 
            let (state', b, a) = fsPop2 state
            return (fsPush (subVals a b) state')

--Multiplies two values on top of stack if they can be multiplied.
doMul :: EDState -> IO EDState
doMul state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (*) error. Multiplication requires two operands!"
        [x] -> error "Operator (*) error. Multiplication requires two operands!"
        vals -> do 
            let (state', a, b) = fsPop2 state
            return (fsPush (multVals a b) state')

--Divides two values on top of stack if they can be divided.
--Errors out if problem happens.
doDiv :: EDState -> IO EDState
doDiv state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (/) error. Division requires two operands!"
        [x] -> error "Operator (/) error. Division requires two operands!"
        vals -> do 
            let (state', b, a) = fsPop2 state
            return (fsPush (divideVals a b) state') 

--Mods two values on top of stack if they can be modded.
--Errors out if problem happens.
doModulo :: EDState -> IO EDState
doModulo state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (%) error. Modulo requires two operands!"
        [x] -> error "Operator (%) error. Modulo requires two operands!"
        vals -> do 
            let (state', b, a) = fsPop2 state
            return (fsPush (modVals a b) state')

--Swaps the top two values at the top of the stack.
doSwap :: EDState -> IO EDState
doSwap state = do 
    let stck = (stack state)
    case stck of 
        [] -> return ( EDState{stack = [], fns = (fns state), vars = (vars state)} )
        [x] -> return ( EDState{stack = [x], fns = (fns state), vars = (vars state)} )
        vals -> do 
            let (state', b, a) = fsPop2 state
            let state'' = fsPush a state' 
            return (fsPush b state'')

--Removes top value from stack.
doDrop :: EDState -> IO EDState
doDrop state = do 
    let stck = (stack state)
    if null stck then return (state)
    else 
        let (state', a) = fsPop state
        in return (state')

--Rotates the top values on the stack.
--If there's 0 or 1 items, nothing happens.
--2 Items is identical to swap.
--3 items performs the rotation.
doRot :: EDState -> IO EDState
doRot state = do 
    let stck = (stack state)
    case stck of 
        [] -> return ( EDState{stack = [], fns = (fns state), vars = (vars state)} )
        [x] -> return ( EDState{stack = [x], fns = (fns state), vars = (vars state)} )
        [x, y] -> return ( EDState{stack = [y, x], fns = (fns state), vars = (vars state)} )
        vals -> do 
            let (state', c, b, a) = fsPop3 state
            let state'' = fsPush a state'
            let state''' = fsPush c state''
            return (fsPush b state''') 

--Duplicates top element of stack or does nothing.
doDup :: EDState -> IO EDState
doDup state = do 
    let stck = (stack state)
    case stck of 
        [] -> return ( EDState{stack = [], fns = (fns state), vars = (vars state)} )
        vals -> do 
            let (state', top) = fsPop state
            let state'' = fsPush top state'
            return (fsPush top state'')

--Checks equality of two elements at the top of the stack.
--Pushes true if they are equal and False if not.
doEqual :: EDState -> IO EDState
doEqual state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (==) error. Equality comparison requires two operands!"
        [x] -> error "Operator (==) error. Equality comparison requires two operands!"
        vals -> do 
            let (state', b, a) = fsPop2 state
            return (fsPush (doEqual' a b) state') 

--Makes sure the types match and then performs the equality operation if so, 
-- otherwise errors out.
doEqual' :: Value -> Value -> Value
doEqual' (BigInteger a) (BigInteger b) = Boolean (a == b)
doEqual' (Integer a) (Integer b) = Boolean (a == b)
doEqual' (Float a) (Float b) = Boolean (a == b)
doEqual' (Double a) (Double b) = Boolean (a == b)
doEqual' (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Boolean ((al == bl) && (acs == bcs))
doEqual' (Char a) (Char b) = Boolean (a == b)
doEqual' (Boolean a) (Boolean b) = Boolean (a == b)
doEqual' (List {items = as, len = al}) (List {items = bs, len = bl}) = Boolean ((al == bl) && (as == bs))
doEqual' _ _ = error "Operator (==) error. Operand types must match for valid comparison!"

--Checks inequality of two elements at the top of the stack.
--Pushes true if they are equal and False if not.
doNotEqual :: EDState -> IO EDState
doNotEqual state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (/=) error. Inequality comparison requires two operands!"
        [x] -> error "Operator (/=) error. Inequality comparison requires two operands!"
        vals -> do 
            let (state', b, a) = fsPop2 state 
            return (fsPush (doNotEqual' a b) state')

--Makes sure the types match and then performs the inequality operation if so, 
-- otherwise errors out.
doNotEqual' :: Value -> Value -> Value
doNotEqual' (BigInteger a) (BigInteger b) = Boolean (a /= b)
doNotEqual' (Integer a) (Integer b) = Boolean (a /= b)
doNotEqual' (Float a) (Float b) = Boolean (a /= b)
doNotEqual' (Double a) (Double b) = Boolean (a /= b)
doNotEqual' (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Boolean (acs /= bcs)
doNotEqual' (Char a) (Char b) = Boolean (a /= b)
doNotEqual' (Boolean a) (Boolean b) = Boolean (a /= b)
doNotEqual' (List {items = as, len = al}) (List {items = bs, len = bl}) = Boolean (as /= bs)
doNotEqual' _ _ = error "Operator (/=) error. Operand types must match for valid comparison!"

--Checks if second to top element is greater than top element of stack.
--Pushes True if true and false if not.
doGreaterThan :: EDState -> IO EDState
doGreaterThan state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (>) error. Greater than comparison requires two operands!"
        [x] -> error "Operator (>) error. Greater than comparison requires two operands!"
        vals -> do 
            let (state', b, a) = fsPop2 state
            return (fsPush (doGreaterThan' b a) state')
                
--Makes sure the types match and then performs the > operation if so, 
-- otherwise errors out.
doGreaterThan' :: Value -> Value -> Value
doGreaterThan' (BigInteger a) (BigInteger b) = Boolean (a > b)
doGreaterThan' (Integer a) (Integer b) = Boolean (a > b)
doGreaterThan' (Float a) (Float b) = Boolean (a > b)
doGreaterThan' (Double a) (Double b) = Boolean (a > b)
doGreaterThan' (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Boolean (acs > bcs)
doGreaterThan' (Char a) (Char b) = Boolean (a > b)
doGreaterThan' (Boolean a) (Boolean b) = Boolean (a > b)
doGreaterThan' (List {items = as, len = al}) (List {items = bs, len = bl}) = Boolean (as > bs)
doGreaterThan' _ _ = error "Operator (>) error. Operand types must match for valid comparison!"

--Checks if second to top element is less than top element of stack.
--Pushes True if true and false if not.
doLessThan :: EDState -> IO EDState
doLessThan state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (<) error. Less than comparison requires two operands!"
        [x] -> error "Operator (<) error. Less than comparison requires two operands!"
        vals -> do 
            let (state', b, a) = fsPop2 state
            return (fsPush (doLessThan' b a) state') 

--Makes sure the types match and then performs the < operation if so, 
-- otherwise errors out.
doLessThan' :: Value -> Value -> Value
doLessThan' (BigInteger a) (BigInteger b) = Boolean (a < b)
doLessThan' (Integer a) (Integer b) = Boolean (a < b)
doLessThan' (Float a) (Float b) = Boolean (a < b)
doLessThan' (Double a) (Double b) = Boolean (a < b)
doLessThan' (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Boolean (acs < bcs)
doLessThan' (Char a) (Char b) = Boolean (a < b)
doLessThan' (Boolean a) (Boolean b) = Boolean (a < b)
doLessThan' (List {items = as, len = al}) (List {items = bs, len = bl}) = Boolean (as < bs)
doLessThan' _ _ = error "Operator (<) error. Operand types must match for valid comparison!"

--Checks if second to top element is greater than equal to the top element of stack.
--Pushes True if true and false if not.
doGreaterThanEqualTo :: EDState -> IO EDState
doGreaterThanEqualTo state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (>=) error. Greater than equal to comparison requires two operands!"
        [x] -> error "Operator (>=) error. Greater than equal to comparison requires two operands!"
        vals -> do 
            let (state', b, a) = fsPop2 state 
            return (fsPush (doGreaterThanEqualTo' b a) state')

--Makes sure the types match and then performs the >= operation if so, 
-- otherwise errors out.
doGreaterThanEqualTo' :: Value -> Value -> Value
doGreaterThanEqualTo' (BigInteger a) (BigInteger b) = Boolean (a >= b)
doGreaterThanEqualTo' (Integer a) (Integer b) = Boolean (a >= b)
doGreaterThanEqualTo' (Float a) (Float b) = Boolean (a >= b)
doGreaterThanEqualTo' (Double a) (Double b) = Boolean (a >= b)
doGreaterThanEqualTo' (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Boolean (acs >= bcs)
doGreaterThanEqualTo' (Char a) (Char b) = Boolean (a >= b)
doGreaterThanEqualTo' (Boolean a) (Boolean b) = Boolean (a >= b)
doGreaterThanEqualTo' (List {items = as, len = al}) (List {items = bs, len = bl}) = Boolean (as >= bs)
doGreaterThanEqualTo' _ _ = error "Operator (>=) error. Operand types must match for valid comparison!"

--Checks if second to top element is less than equal to top element of stack.
--Pushes True if true and false if not.
doLessThanEqualTo :: EDState -> IO EDState
doLessThanEqualTo state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (<=) error. Less than comparison requires two operands!"
        [x] -> error "Operator (<=) error. Less than comparison requires two operands!"
        vals -> do 
            let (state', b, a) = fsPop2 state
            return (fsPush (doLessThanEqualTo' b a) state')

--Makes sure the types match and then performs the <= operation if so, 
-- otherwise errors out.
doLessThanEqualTo' :: Value -> Value -> Value
doLessThanEqualTo' (BigInteger a) (BigInteger b) = Boolean (a <= b)
doLessThanEqualTo' (Integer a) (Integer b) = Boolean (a <= b)
doLessThanEqualTo' (Float a) (Float b) = Boolean (a <= b)
doLessThanEqualTo' (Double a) (Double b) = Boolean (a <= b)
doLessThanEqualTo' (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Boolean (acs <= bcs)
doLessThanEqualTo' (Char a) (Char b) = Boolean (a <= b)
doLessThanEqualTo' (Boolean a) (Boolean b) = Boolean (a <= b)
doLessThanEqualTo' (List {items = as, len = al}) (List {items = bs, len = bl}) = Boolean (as <= bs)
doLessThanEqualTo' _ _ = error "Operator (<=) error. Operand types must match for valid comparison!"

--Performs logical AND function on top two elements of stack.
--If the operands are booleans, then AND is performed.
doAnd :: EDState -> IO EDState
doAnd state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (and) error. Logical AND requires two operands!" 
        [x] -> error "Operator (and) error. Logical AND requires two operands!"
        vals -> do 
            let (state', b, a) = fsPop2 state 
            return (doAnd' state' a b)

--Performs logical AND function on two operands and returns an updated EDState.
-- On failure, an error is thrown.
doAnd' :: EDState -> Value -> Value -> EDState
doAnd' state (Boolean False) (Boolean False) = fsPush (Boolean False) state
doAnd' state (Boolean False) (Boolean True) = fsPush (Boolean False) state
doAnd' state (Boolean True) (Boolean False) = fsPush (Boolean False) state
doAnd' state (Boolean True) (Boolean True) = fsPush (Boolean True) state
doAnd' _ _ _ = error "Operator (and) error. Logical AND requires two boolean types."

--Performs logical OR function on top two elements of stack.
--If the operands are booleans, then OR is performed.
doOr :: EDState -> IO EDState
doOr state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (or) error. Logical OR requires two operands!" 
        [x] -> error "Operator (or) error. Logical OR requires two operands!"
        vals -> do 
            let (state', b, a) = fsPop2 state 
            return (doOr' state' a b) 

--Performs logical OR function on two operands and returns an updated EDState.
-- On failure, an error is thrown.
doOr' :: EDState -> Value -> Value -> EDState
doOr' state (Boolean False) (Boolean False) = fsPush (Boolean False) state
doOr' state (Boolean False) (Boolean True) = fsPush (Boolean True) state
doOr' state (Boolean True) (Boolean False) = fsPush (Boolean True) state
doOr' state (Boolean True) (Boolean True) = fsPush (Boolean True) state
doOr' _ _ _ = error "Operator (or) error. Logical OR requires two boolean types."

--Performs logical XOR function on top two elements of stack.
--If the operands are booleans, then XOR is performed.
doXor :: EDState -> IO EDState
doXor state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (xor) error. Logical XOR requires two operands!" 
        [x] -> error "Operator (xor) error. Logical XOR requires two operands!"
        vals -> do 
            let (state', b, a) = fsPop2 state 
            return (doXor' state' a b) 

--Performs logical XOR function on two operands and returns an updated EDState.
-- On failure, an error is thrown.
doXor' :: EDState -> Value -> Value -> EDState
doXor' state (Boolean False) (Boolean False) = fsPush (Boolean False) state
doXor' state (Boolean False) (Boolean True) = fsPush (Boolean True) state
doXor' state (Boolean True) (Boolean False) = fsPush (Boolean True) state
doXor' state (Boolean True) (Boolean True) = fsPush (Boolean False) state
doXor' _ _ _ = error "Operator (xor) error. Logical XOR requires two boolean types!"

--Performs the logical NOT operator on given boolean 
-- and throws errors when things go wrong.
doNot :: EDState -> IO EDState
doNot state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (not) error. Logical NOT operation requires one operand!"
        vals -> do 
            let (state', top) = fsPop state
            return (doNot' state' top) 

--Performs negation if input value is of type boolean.
doNot' :: EDState -> Value -> EDState
doNot' state (Boolean False) = fsPush (Boolean True) state
doNot' state (Boolean True) = fsPush (Boolean False) state
doNot' _ _ = error "Operator (not) error. Logical NOT requires one boolean type operand!"

--Pushes an item to the end of a list on the stack.
doPush :: EDState -> IO EDState
doPush state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (push) error. Two operands required for push!"
        [x] -> error "Operator (push) error. Two operands required for push!"
        vals -> do 
            let (state', list, val) = fsPop2 state 
            return (doPush' state' list val) 

--Pushes item to list or string.
doPush' :: EDState -> Value -> Value -> EDState
doPush' state (List {items = is, len = l}) valToPush = fsPush ( List {items = (is ++ [valToPush]), len = l + 1} ) state
doPush' state (String {chrs = cs, len = l}) (Char c) = fsPush (String {chrs = cs ++ [c], len = l + 1}) state
doPush' _ _ _ = error "Operator (push) error. Push operator needs a list or string and a value or char to be pushed!"

--Pops an item from the list or string and pushes it to the stack.
doPop :: EDState -> IO EDState
doPop state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (pop) error. Pop operator needs a list to pop from!"
        vals -> do 
            let (state', list) = fsPop state
            return (doPop' state' list) 

--Pops item from list and pushes it to stack.
doPop' :: EDState -> Value -> EDState
--Nothing happens if list is empty.
doPop' state (List {items = [], len = 0}) = fsPush (List {items = [], len = 0}) state
doPop' state (List {items = is, len = l}) = 
    let state' = fsPush (  List {items = init is, len = l - 1} ) state
    in fsPush (last is) state'
doPop' state (String {chrs = "", len = 0}) = fsPush (String {chrs = "", len = 0}) state
doPop' state (String {chrs = cs, len = l}) = 
    let state' = fsPush (String {chrs = init cs, len = l - 1}) state
    in fsPush (Char $ last cs) state'
doPop' _ _ = error "Operator (pop) error. Pop operator needs a list or string to pop items from."

--Pushes an item to the front of a list on the stack.
doFpush :: EDState -> IO EDState
doFpush state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (fpush) error. Two operands required for fpush!"
        [x] -> error "Operator (fpush) error. Two operands required for fpush!"
        vals -> do 
            let (state', list, val) = fsPop2 state 
            return (doFpush' state' list val) 

--Pushes item to list front.
doFpush' :: EDState -> Value -> Value -> EDState
doFpush' state (List {items = is, len = l}) valToPush = fsPush ( List {items = (valToPush : is), len = l + 1} ) state
doFpush' state (String {chrs = cs, len = l}) (Char c) = fsPush (String {chrs = (c : cs), len = l + 1}) state
doFpush' _ _ _ = error "Operator (fpush) error. Operator fpush needs a list/string and a value/char to be pushed to front."

--Pops an item from the front of the list and pushes it to the stack.
doFpop :: EDState -> IO EDState
doFpop state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (fpop) error. List needed!"
        vals -> do 
            let (state', list) = fsPop state
            return (doFpop' state' list)

--Pops item from list and pushes it to stack.
doFpop' :: EDState -> Value -> EDState
--Nothing happens if list is empty.
doFpop' state (List {items = [], len = 0}) = fsPush (List {items = [], len = 0}) state
doFpop' state (List {items = is, len = l}) = 
    let state' = fsPush ( List {items = tail is, len = l - 1} ) state
    in fsPush (head is) state'
--String case.
doFpop' state (String {chrs = cs, len = l}) = 
    let state' = fsPush (String {chrs = tail cs, len = l - 1}) state
    in fsPush (Char $ head cs) state'
doFpop' _ _ = error "Operator (fpop) error. Pop operator needs a list to pop items from."

--Fetches an item from a list of a specific index.
doIndex :: EDState -> IO EDState
doIndex state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (index) error. Two operands required for index!"
        [x] -> error "Operator (index) error. Two operands required for index!"
        vals -> do 
            let (state', list, index) = fsPop2 state 
            return (doIndex' state' list index)

--Retrieves item at index in list or string.
doIndex' :: EDState -> Value -> Value -> EDState
doIndex' state (List {items = [], len = 0}) _ = error "Can't index into empty list."
doIndex' state (String {chrs = "", len = 0}) _ = error "Can't index into empty string."
doIndex' state (List {items = is, len = l}) (Integer index) = 
    let state' = fsPush (List {items = is, len = l}) state
    in fsPush (is !! index) state'
--String case.
doIndex' state (String {chrs = cs, len = l}) (Integer index) = 
    let state' = fsPush (String {chrs = cs, len = l}) state
    in fsPush (Char $ cs !! index) state'
doIndex' _ _ _ = error "Operator (index) error. Index operator needs a list/string and an index value. \n Index must use type Integer to perform an index"

--Takes the length of a list or string at the top 
-- of the stack and pushes resulting length to top of stack.
doLength :: EDState -> IO EDState
doLength state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (length) error. Operand needed for length!"
        vals -> do 
            let top = (fsTop state)
            return (doLength' state top)

--Performs actual length function.
doLength' :: EDState -> Value -> EDState
doLength' state (List {items = _, len = l}) = fsPush (Integer l) state
doLength' state (String {chrs = _, len = l}) = fsPush (Integer l) state
doLength' state _ = error "Operator (length) error. List or string type is needed for length function to work."

--Determines if the list or string at the top
-- of the stack is empty or not.
doIsEmpty :: EDState -> IO EDState
doIsEmpty state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (isEmpty) error. One operand needed!"
        vals -> do 
            let top = (fsTop state)
            return (doIsEmpty' state top) 

--Performs actual length function.
doIsEmpty' :: EDState -> Value -> EDState
doIsEmpty' state (List {items = is, len = l}) = fsPush (Boolean $ null is) state
doIsEmpty' state (String {chrs = cs, len = l}) = fsPush (Boolean $ null cs) state
doIsEmpty' state _ = error "Operator (isEmpty) error. List or string type is needed to test for emptyness."

--Sets the string or list at the top of the stack to empty.
doClear :: EDState -> IO EDState
doClear state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (clear) error. One operand needed!"
        vals -> do 
            let (state', list) = fsPop state
            return (doClear' state' list) 

--Performs clear operation.
doClear' :: EDState -> Value -> EDState
doClear' state (List {items = _, len = _}) = fsPush (List {items = [], len = 0}) state
doClear' state (String {chrs = _, len = _}) = fsPush (String {chrs = "", len = 0}) state
doClear' state _ = error "Operator (clear) error. List or string is needed for clear to occur."

--Used to turn a value of one type into another.
doCast :: EDState -> IO EDState
doCast state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (cast) error. Two operands required for cast!"
        [x] -> error "Operator (cast) error. Two operands required for cast!"
        vals -> do 
            let (state', val, castType) = fsPop2 state
            return (doCast' state' val castType) 

--Performs the actual cast operation.
doCast' :: EDState -> Value -> Value -> EDState

doCast' state (Boolean b) (String {chrs = "Integer", len = _}) = fsPush (Integer (if b then 1 else 0)) state
doCast' state (Boolean b) (String {chrs = "BigInteger", len = _}) = fsPush (BigInteger (if b then 1 else 0)) state
doCast' state (Boolean b) (String {chrs = "String", len = _}) = fsPush (String {chrs = show b, len = length $ show b}) state
doCast' state (Boolean b) (String {chrs = "Boolean", len = _}) = fsPush (Boolean b) state --Do nothing case.

doCast' state (BigInteger n) (String {chrs = "String", len = _}) = fsPush (String {chrs = show n, len = length $ show n}) state
doCast' state (BigInteger n) (String {chrs = "Integer", len = _}) = fsPush (Integer (fromIntegral n :: Int)) state
doCast' state (BigInteger n) (String {chrs = "BigInteger", len = _}) = fsPush (BigInteger n) state --Do nothing case.
doCast' state (BigInteger n) (String {chrs = "Float", len = _}) = fsPush (Float (fromIntegral n :: Float)) state
doCast' state (BigInteger n) (String {chrs = "Double", len = _}) = fsPush (Double (fromIntegral n :: Double)) state

doCast' state (Integer n) (String {chrs = "String", len = _}) = fsPush (String {chrs = show n, len = length $ show n}) state
doCast' state (Integer n) (String {chrs = "Integer", len = _}) = fsPush (Integer n) state --Do nothing case
doCast' state (Integer n) (String {chrs = "BigInteger", len = _}) = fsPush (BigInteger (fromIntegral n :: Integer)) state 
doCast' state (Integer n) (String {chrs = "Float", len = _}) = fsPush (Float (fromIntegral n :: Float)) state
doCast' state (Integer n) (String {chrs = "Double", len = _}) = fsPush (Double (fromIntegral n :: Double)) state

doCast' state (Float n) (String {chrs = "String", len = _}) = fsPush (String {chrs = show n, len = length $ show n}) state
doCast' state (Float n) (String {chrs = "Integer", len = _}) = fsPush (Integer (truncate n)) state
doCast' state (Float n) (String {chrs = "BigInteger", len = _}) = fsPush (BigInteger (floor n :: Integer)) state 
doCast' state (Float n) (String {chrs = "Float", len = _}) = fsPush (Float n) state --Do nothing case.
doCast' state (Float n) (String {chrs = "Double", len = _}) = fsPush (Double (realToFrac n :: Double)) state

doCast' state (Double n) (String {chrs = "String", len = _}) = fsPush (String {chrs = show n, len = length $ show n}) state
doCast' state (Double n) (String {chrs = "Integer", len = _}) = fsPush (Integer (truncate n)) state
doCast' state (Double n) (String {chrs = "BigInteger", len = _}) = fsPush (BigInteger (floor n :: Integer)) state 
doCast' state (Double n) (String {chrs = "Float", len = _}) = fsPush (Float (realToFrac n :: Float)) state
doCast' state (Double n) (String {chrs = "Double", len = _}) = fsPush (Double n) state --Do nothing case.

doCast' state (String {chrs = cs, len = l}) (String {chrs = "String", len = _}) = fsPush (String {chrs = cs, len = l}) state --Do nothing case.

doCast' state (Char c) (String {chrs = "String", len = _}) = fsPush ( String {chrs = ("" ++ [c]), len = length ("" ++ [c])} ) state --Char to string cast.

doCast' state (String {chrs = cs, len = l}) (String {chrs = "Integer", len = _}) = 
    let mbyInt = readMaybe cs :: Maybe Int
        parsed = case mbyInt of 
                    Just val -> val 
                    Nothing -> error "Operator (cast) error. Failed to convert String to type Integer."
    in fsPush (Integer parsed) state

doCast' state (String {chrs = cs, len = l}) (String {chrs = "BigInteger", len = _}) = 
    let mbyBigInt = readMaybe cs :: Maybe Integer
        parsed = case mbyBigInt of 
                    Just val -> val 
                    Nothing -> error "Operator (cast) error. Failed to convert String to type BigInteger."
    in fsPush (BigInteger parsed) state

doCast' state (String {chrs = cs, len = l}) (String {chrs = "Float", len = _}) = 
    let mbyFlt = readMaybe cs :: Maybe Float
        parsed = case mbyFlt of 
                    Just val -> val 
                    Nothing -> error "Operator (cast) error. Failed to convert String to type Float."
    in fsPush (Float parsed) state

doCast' state (String {chrs = cs, len = l}) (String {chrs = "Double", len = _}) = 
    let mbyDbl = readMaybe cs :: Maybe Double
        parsed = case mbyDbl of 
                    Just val -> val 
                    Nothing -> error "Operator (cast) error. Failed to convert String to type Double."
    in fsPush (Double parsed) state

doCast' state val _ = error "Operator (cast) error. Second argument of cast needs to be string."

--Prints top element of stack. This element must be a string or it freaks out.
doPrintLine :: EDState -> IO EDState
doPrintLine state = do 
    let stck = (stack state)
    if (null stck) 
        then error "Operator (printLine) error. Can't print from empty stack!"
        else case (head stck) of 
            String {chrs = cs, len = l} -> putStrLn cs 
            _ -> error "Operator (printLine) error. Top of stack must be a string to be printed!"
    return state

--Reads a line from stdin, and pushes it onto stack.
doReadLine :: EDState -> IO EDState
doReadLine state = do 
    let stck = (stack state)
    input <- getLine
    return (EDState{stack = ((String {chrs = input, len = length input}) : stck), fns = (fns state), vars = (vars state)})

--Determines if a character at the top 
-- of the stack is a whitespace character, 
-- pushes true if yes and false if no.
doIsWhite :: EDState -> IO EDState
doIsWhite state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (isWhitespace) error. Operand on stack needed!"
        vals -> case (head vals) of 
            Char c -> return (fsPush (Boolean (isSpace c)) state)
            _ -> error "Operator (isWhitespace) error. Type to be analyzed needs to be type Char!"

--Determines if a list contains a member.
doContains :: EDState -> IO EDState
doContains state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (contains) error. Two operands on stack needed!"
        [x] -> error "Operator (contains) error. Two operands on stack needed!"
        vals -> do 
            let (top, secondToTop) = ((head stck), (head $ tail stck))
            let contains = case (top, secondToTop) of 
                                (v, List {items = is, len = _}) -> v `elem` is
                                (Char c, String {chrs = cs, len = _}) -> c `elem` cs
                                (_, _) -> error "Operator (contains) error. List or string needed to asses if item is contained within."
            return (fsPush (Boolean contains) state)

--Changes an item at a given index in a list to a new item on the stack.
doChangeItemAt :: EDState -> IO EDState
doChangeItemAt state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (changeItemAt) error. Three operands needed!"
        [x] -> error "Operator (changeItemAt) error. Three operands needed!"
        [x, y] -> error "Operator (changeItemAt) error. Three operands needed!"
        vals -> do 
            let (state', chngLs, chngItem, index) = fsPop3 state
            case (chngLs, chngItem, index) of 
                (List {items = is, len = l}, v, Integer i) -> return (fsPush ( List { items = (changeItem [] is 0 i v), len = l } ) state')
                (_, _, _) -> error "Operator (changeItemAt) error. List, value, and Integer type in that order needed on stack."

--Rebuilds list with altered item if possible. 
--THIS IS POORLY OPTIMIZED AND WILL SUCK ON A LARGE LIST SO MAKE IT BETTER LATER!
changeItem :: [Value] -> [Value] -> Int -> Int -> Value -> [Value]
changeItem acc [] curr desiredIndex insVal = acc
changeItem acc (x:xs) curr desiredIndex insVal
    |    curr == desiredIndex = (acc ++ [insVal] ++ xs)
    |    otherwise = changeItem (acc ++ [x]) (xs) (curr + 1) desiredIndex insVal

--Raises one Float or Double to another Float or Double 
--and returns as such, consuming the original two numbers.
doPow :: EDState -> IO EDState
doPow state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (pow) error. Two operands needed!"
        [x] -> error "Operator (pow) error. Two operands needed!"
        vals -> do 
            let (state', base, expnt) = fsPop2 state
            case (base, expnt) of 
                (Float bs, Float ex) -> return (fsPush (Float (bs ** ex)) state')
                (Double bs, Double ex) -> return (fsPush (Double (bs ** ex)) state')
                (_, _) -> error "Operator (pow) error.\nOperands need to be type Float Float or Double Double!\nCan't mix types and only Float or Double types are valid!"

-- performs the operation identified by the string. for example, doOp state "+"
-- will perform the "+" operation, meaning that it will pop two values, sum them,
-- and push the result. 
doOp :: String -> EDState -> IO EDState
-- here's how we turn the strings into their corresponding operation. 
doOp "+"  = doAdd
doOp "-"  = doSub
doOp "*"  = doMul
doOp "/"  = doDiv  
doOp "swap"  = doSwap  
doOp "drop"  = doDrop  
doOp "rot"  = doRot  
doOp "dup"  = doDup 
doOp "=="  = doEqual 
doOp "/="  = doNotEqual 
doOp ">"  = doGreaterThan 
doOp "<"  = doLessThan 
doOp ">="  = doGreaterThanEqualTo 
doOp "<=" = doLessThanEqualTo 
doOp "%" = doModulo
doOp "++" = doConcat
doOp "and" = doAnd 
doOp "or" = doOr 
doOp "xor" = doXor
doOp "not" = doNot
doOp "pow" = doPow --Exponential operation
--List operations
doOp "push" = doPush
doOp "p" = doPush --Alias for push
doOp "pop" = doPop
doOp "po" = doPop --Alias for pop
doOp "fpush" = doFpush
doOp "fp" = doFpush --Alias for fpush
doOp "fpop" = doFpop
doOp "fpo" = doFpop --Alias for fpop
doOp "index" = doIndex
doOp "length" = doLength
doOp "len" = doLength --Alias for length
doOp "isEmpty" = doIsEmpty
doOp "clear" = doClear
doOp "contains" = doContains
doOp "changeItemAt" = doChangeItemAt --Changes item in list at a specified index.
doOp "isWhitespace" = doIsWhite --Checks if character is whitespace.
--Type stuff
doOp "cast" = doCast
--IO stuff
doOp "printLine" = doPrintLine
doOp "readLine" = doReadLine

-- Error thrown if reached here.
doOp op = error $ "unrecognized word: " ++ op 

astNodeToString :: AstNode -> String
astNodeToString (Terminal (Word w)) = w

makeVar :: EDState -> String -> IO EDState
makeVar state varName = 
    let lkup = M.lookup varName (vars state)
    --Throw error if variable exists. Otherwise, make variable by inserting it into hash table.
    in case lkup of
        Just _ -> error "Variable Mak Error: Variable already exists."
        Nothing -> do 
            let top = fsTop state
            let vars' = M.insert varName top (vars state)
            return (EDState{stack = (stack state), fns = (fns state), vars = vars'})

--Comapres types in order to enforce static typing when mutating variables.
compareTypesForMut :: Value -> Value -> Bool
compareTypesForMut (Boolean _) (Boolean _) = True
compareTypesForMut (BigInteger _) (BigInteger _) = True
compareTypesForMut (Integer _) (Integer _) = True
compareTypesForMut (Double _) (Double _) = True
compareTypesForMut (Float _) (Float _) = True
compareTypesForMut (String {chrs = _, len = _}) (String {chrs = _, len = _}) = True
compareTypesForMut (Char _) (Char _) = True
compareTypesForMut (List {items = _, len = _}) (List {items = _, len = _}) = True
compareTypesForMut _ _ = False

--Changes variable to new value if it can be mutated.
mutateVar :: EDState -> String -> IO EDState
mutateVar state varName = do 
    let lkupVal = M.lookup varName (vars state)
    let newVal = fsTop state
    
    --If variable exists it can be mutated. Otherwise, an error is thrown.
    case lkupVal of
        Just value -> if compareTypesForMut value newVal then 
            let vars' = M.insert varName newVal (vars state)
            in return ( EDState{stack = (stack state), fns = (fns state), vars = vars'} )
            else error "Variable Mut Error: Can't mutate variable to different type."
        Nothing -> error "Variable Mut Error: Variable doesn't exist or was deleted"

funcDef :: EDState -> String -> AstNode -> EDState
funcDef state funcName funcBod = 
    let look = M.lookup funcName (fns state)
    in case look of 
        Just bod -> error "Function Def Error: Function of same name already exists" 
        Nothing -> let fns' = M.insert funcName funcBod (fns state)
                   in EDState{stack = (stack state), fns = fns', vars = (vars state)}

funcCall :: EDState -> String -> (EDState, AstNode)
funcCall state funcName = 
    let look = M.lookup funcName (fns state)
    in case look of 
        Just body -> (state, body)
        Nothing -> error "Function Call Error: Function isn't defined."

--Runs through the code and executes all nodes of the AST.
doNode :: AstNode -> EDState -> IO EDState

-- Runs true branch if top of stack is true 
--and false branch if top of stack is false.
doNode If { ifTrue = trueBranch, ifFalse = falseBranch } state = do
    let isStackEmpty = (null (stack state))
    let top = if isStackEmpty 
        then error "If statement error:\nNo boolean value for if to check because stack is empty." 
        else fsTop state

    --Runs true branch if top is true, false if false, and errors out otherwise.
    case top of 
        (Boolean True) -> doNode trueBranch state
        (Boolean False) -> doNode falseBranch state
        _ -> error "If statement error:\nIf statement requires top of stack to be type Boolean to perform valid branching!"

--Patterm matches function definition.
doNode (Expression((Function {funcCmd = cmd, funcName = name, funcBod = body}):rest)) state =
    case (astNodeToString cmd) of 
        "def" -> doNode (Expression(rest)) (funcDef state (astNodeToString name) body)
        "call" -> do 
                    let (state', funcBod) = funcCall state (astNodeToString name)
                    state'' <- (doNode funcBod state')
                    doNode (Expression(rest)) state''
        _ -> error "Function Error: Invalid function command given. Valid: def, call"

--Runs all the different cases of variable actions.
doNode (Expression((Variable{varName = name, varCmd = cmd}):rest)) state =
    case (astNodeToString cmd) of
        "mak" -> do 
            let stackIsEmpty = null (stack state)
            if stackIsEmpty
                then error "Variable Mak Error: Can't create variable when stack is empty."
                else do
                    state' <- (makeVar state (astNodeToString name))
                    doNode (Expression rest) state'
                           
        "get" -> let lkup = M.lookup (astNodeToString name) (vars state) 
                 in case lkup of
                    Just value -> let stack' = fsPush value state
                              in doNode (Expression rest) stack' 
                    Nothing -> error "Variable Get Error: Variable doesn't exist or was deleted"

        "del" -> let lkup = M.lookup (astNodeToString name) (vars state) 
                 in case lkup of
                    Just value -> let vars' = M.delete (astNodeToString name) (vars state)
                           in doNode (Expression rest) (EDState{stack = (stack state), fns = (fns state), vars = vars'})
                    Nothing -> error "Variable Del Error: Variable doesn't exist" 

        "mut" -> do 
            let stackIsEmpty = null (stack state)
            if stackIsEmpty
                then error "Variable Mut Error: Can't mutate variable when stack is empty."
                else do 
                    state' <- (mutateVar state (astNodeToString name))
                    doNode (Expression rest) state'

        _ -> error "Variable Command Error: Invalid variable command given. Valid: mak, get, mut, del"

--Runs while loop.                                                                                                                      
doNode ( While loopBody ) state = do
    let stackIsEmpty = null (stack state)
    let top = if stackIsEmpty 
        then error "While Loop error:\nNo boolean value for while loop to check because stack is empty." 
        else fsTop state

    --Creates new stack if loop body runs.
    -- Otherwise newState is same as state.
    -- Errors out if top of stack isn't a boolean type.
    newState <- case top of 
        (Boolean True) -> doNode (loopBody) state
        (Boolean False) -> return (state)
        _ -> error "While Loop error:\nTop of stack needs to be type Boolean for loop to see if it needs to run again!"

    let stackIsEmpty' = null (stack newState)
    let top' = if stackIsEmpty'
        then error "While Loop error:\nNo boolean value for while loop to check because stack is empty."
        else fsTop newState

    --If loop ran and can run again, it's run again, 
    -- otherwise, newState is returned.
    case top' of 
        (Boolean True) -> doNode (While loopBody) newState
        (Boolean False) -> return newState
        _ -> error "While Loop error:\nTop of stack needs to be type Boolean for loop to see if it needs to run again!"

-- doing a terminal changes depending on whether it's a word or a number. 
-- if it's a number, push it...
doNode ( Terminal ( Val v ) ) state = return (fsPush v state)

-- ...if it's a word, execute the operation
doNode ( Terminal ( Word o ) ) state = doOp o state

-- "doing" an empty expression does nothing
doNode ( Expression [] ) state = return state

-- "doing" a non-empty expression tries to execute every node in the expression
doNode ( Expression ( first:rest ) ) state = do  
    stateAfterFirst <- doNode first state
    doNode (Expression (rest)) stateAfterFirst                                     

-- arguments:
--  alreadyParsed :: [AstNode]: a list of nodes parsed so far. Starts empty.
--  tokens :: [Token]: a list of tokens remaining to be parsed
--  terminators :: [String]: a list of words that will stop parsing. 
-- How it works: 
--  * if the next token is a terminator, we're done parsing. This happens when we're in an 
--    if statement and we see a ';' or 'else' for example. 
--  * if we see the word "if", call parseIf, which reads the if branch and else branch. 
--    afterwards, we parse the remainder of the program and paste the result onto what we got 
--    when we parsed the if. 
--  * if we see the word "while" call parseWhile. This works in much the same way as parseIf
--  * if none of the above, we found a random operation or number. just append whatever we found 
--    to the alreadyParsed list and keep going. 
parseExpression' :: [AstNode] -> [Token] -> [String] -> ( [AstNode], [Token], Maybe Token )

-- if there are no more tokens, we need to check if we have terminators.
-- if we were expecting a terminator and there isn't one, that's an error. 
parseExpression' alreadyParsed [] terminators =
    -- this is the base case: nothing to parse
    if null terminators then ( alreadyParsed, [], Nothing ) 
    -- error case 
    else error ( "ended expression without finding one of: " ++ intercalate ", " terminators )

-- if tokens remain, keep parsing
parseExpression' alreadyParsed ( token:tokens ) terminators 
    -- found a terminator: stop parsing and return. 
    | token `elem` map Word terminators = ( alreadyParsed, tokens, Just token )

    -- found an if-statement: remove the "if" token, parse the true and false branches, and 
    -- then parse whatever is after the if-statement.
    | token == Word "if" = 
        let ( trueBranch, falseBranch, remTokens ) = parseIf tokens
            newParsed = alreadyParsed ++ [If{ifTrue = trueBranch, ifFalse = falseBranch}]
        in parseExpression' newParsed remTokens terminators
            
    -- found a while-statement: remove the "while", parse the body, then parse whatever is after
    | token == Word "while" = 
        let (bod, remTokens) = parseWhile tokens
            newParsed = alreadyParsed ++ [While(bod)]
        in parseExpression' newParsed remTokens terminators                                              
    
    --Parse function definition.
    | token == Word "func" = 
        let (cmd, name, bod, remTokens) = parseFuncOp tokens
            newParsed = alreadyParsed ++ [Function{funcCmd = cmd, funcName = name, funcBod = bod}]
        in parseExpression' newParsed remTokens terminators

    | token == Word "var" = 
        let (varAct, variableName, remTokens) = parseVarAction tokens
            newParsed = alreadyParsed ++ [Variable{varName = variableName, varCmd = varAct}]
        in parseExpression' newParsed remTokens terminators
        
    -- no special word found. We are parsing a list of operations. Keep doing this until 
    -- there aren't any. 
    | otherwise = parseExpression' (alreadyParsed ++ [Terminal(token)]) (tokens) (terminators)

-- takes the result of parseExpression' and wraps it in an Expression constructor
parseExpression :: [Token] -> AstNode
parseExpression tokens = 
    let (nodes, toks, potTok) = parseExpression' [] tokens []
    in Expression(nodes)

--Custom trace function used during debugging. Made by Grant.
traceThing :: (Show a) => a -> a 
traceThing x = traceShow x x 

parseVarAction :: [Token] -> (AstNode, AstNode, [Token])
parseVarAction (token:tokens) = 
    let (varInfo, remTokens, terminator) = parseExpression' [] tokens [";"]
        varAction = Terminal token
        varName = if not $ null (tail varInfo) 
            then error "Malformed variable command" else head varInfo
        in (varAction, varName, remTokens)

--Parses a function definition.
parseFuncOp :: [Token] -> (AstNode, AstNode, AstNode, [Token])
parseFuncOp (command:name:tokens) =
    let (funcBody, remTokens, terminator) = parseExpression' [] tokens [";"]
        funcCommand = Terminal command
        funcName = Terminal name
        in (funcCommand, funcName, Expression(funcBody), remTokens)

-- we just saw an "if". now we have to build an "If" AstNode.
-- returns the two branches and the remaining tokens. 
-- ( ifTrue, ifFalse, remainingTokens ). 
parseIf :: [Token] -> ( AstNode, AstNode, [Token] ) 
parseIf tokens = 
    let ( trueBranch, remainingTokens, terminator ) = parseExpression' [] tokens [ "else", ";" ]
        (falseBranch, remTokens) = if terminator == (Just $ Word "else") 
            then parseElse remainingTokens 
            else (Expression([]), remainingTokens)
    in (Expression(trueBranch), falseBranch, remTokens)

-- we just saw an "else". now finish the ifFalse part of the If node. This one only needs to 
-- return the "false" branch of the if statement, which is why there is only one [AstNode] in 
-- the return value. 
parseElse :: [Token] -> (  AstNode, [Token] )                                                               
parseElse tokens = 
    let (ifFalse, remTokens, terminator) = parseExpression' [] tokens [";"]
    in (Expression(ifFalse), remTokens)

-- parsing a while loop is similar to parsing an if statement. 
parseWhile :: [Token] -> ( AstNode, [Token] )
-- if we reach the end of our tokens without closing the loop, that's an error 
parseWhile [] = error "while without closing semicolon."
-- otherwise, parse the loop body until reaching the ";" 
parseWhile tokens = let (loopBod, remTokens, terminator) = parseExpression' [] tokens [";"]
                    in (Expression(loopBod), (remTokens))
    
-- create a new interpreter
fsNew :: IO EDState
fsNew = return EDState { stack = [], fns = M.empty, vars = M.empty }

-- push a new value onto the stack
fsPush :: Value -> EDState -> EDState
fsPush val state = EDState { stack = (val : (stack state)), fns = (fns state), vars = (vars state)}

--Removes value from top of stack, returning it.
fsPop :: EDState -> ( EDState, Value )
fsPop state = 
    let top = head (stack state) 
        newStack = tail (stack state)  
    in  ( EDState { stack = newStack, fns = (fns state), vars = (vars state) }, top )

--Removes the top two elements from the stack, returning them.
fsPop2 :: EDState -> ( EDState, Value, Value )
fsPop2 state = 
    let (state', top) = fsPop state
        (state'', secondToTop) = fsPop state'
    in  (EDState {stack = (stack state''), fns = (fns state), vars = (vars state)}, secondToTop, top)

--Removes top three elements from stack.
fsPop3 :: EDState -> ( EDState, Value, Value, Value )
fsPop3 state = 
    let (state', top) = fsPop state
        (state'', secondToTop) = fsPop state'
        (state''', thirdToTop) = fsPop state''
    in (EDState {stack = (stack state'''), fns = (fns state), vars = (vars state)}, thirdToTop, secondToTop, top)

--Returns value at top of stack. 
fsTop :: EDState -> Value 
fsTop state = head (stack state) 

--Counts the number of decimal points in a string.
decCount :: String -> Int
decCount "" = 0
decCount (x:xs) = if x == '.' then 1 + decCount xs else decCount xs

isNum' :: String -> Bool -> Bool 
isNum' "" isNum  = isNum  
isNum' (x:xs) isNum =
    let nums = "0123456789"
    in if not (x `elem` nums || x == '.' || x == 'e' || x == '-') 
        then isNum' xs False
        else isNum' xs isNum

--Determines if a string is a valid number.
isNum :: String -> Int
isNum "" = -1
isNum numStr = 
    let containsValidChars = isNum' numStr True
        decimalPoints = decCount numStr
        minusSigns = length $ filter (=='-') numStr
        exponentCount = length $ filter (=='e') numStr
    in if containsValidChars 
        then case (decimalPoints, minusSigns, exponentCount) of
            (0, 0, 0) -> 0
            (1, 0, 0) -> 1
            (1, 0, 1) -> 1
            (1, 1, 1) -> 1
            _ -> -1
        else -1 

-- Used to turn the strings into values and other tokens.
lexToken :: String -> Token
lexToken t
    | t == "true" || t == "True" = Val $ Boolean True  --Boolean cases.
    | t == "false" || t == "False" = Val $ Boolean False
    | t == "[]" = Val $ List {items = [], len = 0}  --Empty list case.
    | (head t) == '"' && (last t) == '"' =
        let str = read t :: String
        in Val $ String { chrs = str, len = length str }  --String case                                                                  
    | (head t) == '\'' && (last t) == '\'' && length t == 3 = Val $ Char (read t :: Char) --Char case
    | (last t == 'b') && ((isNum (if head t == '-' then tail $ init t else init t)) == 0) = Val $ BigInteger (read (init t) :: Integer) --BigInteger case
    | (last t == 'd') && ((isNum (if head t == '-' then tail $ init t else init t)) == 1) = Val $ Double (read (init t) :: Double) -- Double case
    | (isNum (if head t == '-' then tail t else t)) == 0 = Val $ Integer (read t :: Int) --Int Case
    | (isNum (if head t == '-' then tail t else t)) == 1 = Val $ Float (read t :: Float) --Float case
    | otherwise = Word t                             

-- Takes a whole program and turns it into a list of tokens. Calls "lexToken"
tokenize :: String -> [Token]
tokenize code = map lexToken $ tokenize' code 

--This code makes creating a list of strings 
-- from the code easier in the above function call.
tokenize' :: String -> [String]
tokenize' "" = []
tokenize' str = tokenize'' str "" [] False False

--This function does the heavy lifting of spitting up the code into tokens.
tokenize'' :: String -> String -> [String] -> Bool -> Bool -> [String]
tokenize'' "" _ _ False True = error "Parse Error: Code ended without array being closed." --Error case if array isn't closed.          
tokenize'' "" _ _ True False = error "Parse Error: Code ended without string being closed." --Error case for non closed string.         
tokenize'' "" currStr strs False False = strs ++ (if null currStr then [] else [currStr]) --Parsing is complete case.

tokenize'' (('\''):c:('\''):xs) currStr strs False False = tokenize'' xs currStr (strs ++ [ "\'" ++ [c] ++ "\'" ] ) False False --Character case.

tokenize'' (('\"'):xs) currStr strs False False = tokenize'' xs (currStr ++ ['\"']) strs True False --String enter case.
tokenize'' (('\"'):xs) currStr strs True False = tokenize'' xs [] (strs ++ [currStr ++ ['\"']]) False False --Exiting string case.
tokenize'' (('\\'):('\"'):xs) currStr strs True False = tokenize'' xs (currStr ++ "\\\"") (strs) True False --In string case for quotes in quotes.
--tokenize'' (('\\'):('{'):xs) currStr strs True False = tokenize'' xs (currStr ++ ['\{']) strs True False --In string case for left array bracket in a string. FIX LATER MAYBE???????
--tokenize'' (('\\'):('}'):xs) currStr strs True False = tokenize'' xs (currStr ++ ['\}']) strs True False --In string case for right array bracket in a string.

tokenize'' (x:xs) currStr strs True False = tokenize'' xs (currStr ++ [x]) strs True False --In string case

--ADD ARRAY TOKENIZATION LATER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--General parsing case.
tokenize'' (x:xs) currStr strs False False = if not $ isSpace x 
    then tokenize'' xs (currStr ++ [x]) strs False False
    else tokenize'' xs [] (strs ++ (if null currStr then [] else [currStr])) False False

-- removes comments from a token stream. comments are between /' and '/
-- arguments:
--  * the first bool tells us whether we are in a comment or not. starts false.
--  * the first token list is the tokens that are not inside of comments. starts empty.
--  * the last list are the remaining tokens 
removeComments :: Bool -> [Token] -> [Token] -> [Token]

-- If the first argument is 'true', we're inside a comment. but the [] means no more tokens.
removeComments True _ [] = error "ended comment while it's still open. need closing '/ ."  

-- If we finish all the tokens and are not in a comment, there's nothing else to do
-- except reversing the nonComments tokens (because we've been appending to the front)
removeComments False nonComments [] = reverse nonComments

-- If we're in a comment and we find '/, we close the comment and continue
removeComments True nonComments ( Word ("'/"):xs ) = removeComments False nonComments xs

-- If we're in a comment, ignore whatever token comes next 
removeComments True nonComments ( _:xs ) = removeComments True nonComments xs

-- If we're not in a comment and we find /', start the comment 
removeComments False nonComments ( Word ("/'"):xs ) = removeComments True nonComments xs

-- If we're not in a comment, add the token to the nonComment tokens 
removeComments False nonComments ( x:xs ) = removeComments False (x:nonComments) xs

--Prints the end stack in a way that's more formal and nice looking.
--Each element of the stack is printed on a seperate line growing downwards. 
--If the stack is empty, nothing is printed.
printStack :: [Value] -> IO ()
printStack [] = return ()
printStack ((List {items = is, len = l}):xs) = 
    let prnt = if l < 16 then show is else (init $ show $! take 15 is) ++ ", ...]"
    in putStrLn prnt >> printStack xs
printStack ((String {chrs = cs, len = l}):xs) =
    let pr = if l < 256 then show cs else (init $ show $ take 255 cs) ++ "...\""
    in putStrLn pr >> printStack xs
printStack (x:xs) = print x >> printStack xs

main :: IO ()
main = do
    args <- getArgs

    inputFile <- if (not $ null args) 
        then openFile (args !! 0) ReadMode 
        else error "Please provide an EcksDee code file to parse!"

    -- get all the code passed to STDIN as a giant string 
    code <- hGetContents inputFile

    -- convert it into a list of tokens
    let tokens = removeComments False [] ( tokenize code )

    --Parse and run AST, printing result.
    let ast = parseExpression tokens
    stateInit <- fsNew
    finalState <- (doNode ast stateInit)
    printStack $ reverse $ (stack finalState) 

    hClose inputFile