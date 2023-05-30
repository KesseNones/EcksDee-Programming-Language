--Jesse A. Jones
--Version: 2023-05-30.12
--Toy Programming Language Named EcksDee

{-
    ISSUES:
        -Casting is still needed.
        -IO needed.
-}

import Data.List
import Data.Char
import Data.Maybe 
import Debug.Trace
import Text.Read (readMaybe)
import qualified Data.Map.Strict as M

data Value =                                         
        BigInteger Integer
    |   Integer Int
    |   Float Float
    |   Double Double
    |   String String
    |   Char Char
    |   Boolean Bool
    |   List [Value]
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
} deriving ( Show )

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

--Concatenates two strings.
doConcat' :: Value -> Value -> Value
doConcat' (String a) (String b) = String (a ++ b)
doConcat' _ _ = error "Operator (++) error. \n Can't perform concatenation on types that aren't Strings."

doConcat :: EDState -> EDState
doConcat EDState{stack = [], fns = fs, vars = vs} = 
    error "Operator (++) error. Concatenation requires two operands!"
doConcat EDState{stack = [x], fns = fs, vars = vs} = 
    error "Operator (++) error. Concatenation requires two operands!"
doConcat state =
    let (state', a, b) = fsPop2 state
    in fsPush (doConcat' a b) state'

doAdd :: EDState -> EDState
doAdd EDState{stack = [], fns = fs, vars = vs} = 
    error "Operator (+) error. Addition requires two operands!"
doAdd EDState{stack = [x], fns = fs, vars = vs} = 
    error "Operator (+) error. Addition requires two operands!"
doAdd state = 
    let ( state', b, a  ) = fsPop2 state 
    in fsPush (addVals a b) state' 
-- we need to pop 2 values so we can add them.
-- we will pop 2 values in all the below operations. 
-- you can streamline this by defining a helper function "binary_op" if you want.
-- it can take a function with type Int -> Int -> Int and apply it to the top 
-- two values on the stack, pushing the result. 

-- apply the - operation: pop 2 values, subtract them, push the result. 1 2 - -> -1
doSub :: EDState -> EDState
doSub EDState{stack = [], fns = fs, vars = vs} = 
    error "Operator (-) error. Subtraction requires two operands!"
doSub EDState{stack = [x], fns = fs, vars = vs} = 
    error "Operator (-) error. Subtraction requires two operands!"
doSub state =
    let (stateNew, b, a) = fsPop2 state
    in fsPush (subVals a b) stateNew

-- apply the * operation: pop 2 values, multiply them, push the result. 3 4 * -> 12
doMul :: EDState -> EDState
doMul EDState{stack = [], fns = fs, vars = vs} = 
    error "Operator (*) error. Multiplication requires two operands!"
doMul EDState{stack = [x], fns = fs, vars = vs} = 
    error "Operator (*) error. Multiplication requires two operands!"
doMul state = 
    let (stateNew, b, a) = fsPop2 state
    in fsPush (multVals a b) stateNew

-- apply the / operation: pop 2 values, divide them, push the result. 4 2 / -> 2
doDiv :: EDState -> EDState
doDiv EDState{stack = [], fns = fs, vars = vs} = 
    error "Operator (/) error. Division requires two operands!"
doDiv EDState{stack = [x], fns = fs, vars = vs} = 
    error "Operator (/) error. Division requires two operands!"
doDiv state = 
    let (stateNew, b, a) = fsPop2 state
    in fsPush (divideVals a b) stateNew

--Apply modulo operation
doModulo :: EDState -> EDState
doModulo EDState{stack = [], fns = fs, vars = vs} = 
    error "Operator (%) error. Modulo requires two operands!"
doModulo EDState{stack = [x], fns = fs, vars = vs} = 
    error "Operator (%) error. Modulo requires two operands!"
doModulo state =
    let (state', b, a) = fsPop2 state
    in fsPush (modVals a b) state'

-- apply the swap operation. pop 2 values, re-push them in reverse order. 1 2 swap -> 2 1 
doSwap :: EDState -> EDState 
doSwap EDState{stack = [], fns = fs, vars = vs} = 
    EDState{stack = [], fns = fs, vars = vs}
doSwap EDState{stack = [x], fns = fs, vars = vs} = 
    EDState{stack = [x], fns = fs, vars = vs}
doSwap state = 
    let (stateNew, b, a) = fsPop2 state
        stateNewer = fsPush a stateNew
    in fsPush b stateNewer

-- apply the drop operation. pop 1 value. 1 2 3 -> 1 2 
-- does nothing if stack is empty 
doDrop :: EDState -> EDState
doDrop state = 
    if null $ stack state then state
    else 
        let (stateNew, a) = fsPop state
        in stateNew    

-- apply the rot operation. rotates the top three right: 1 2 3 -> 3 1 2 
-- does nothing if stack is empty or size 1
-- same as swap if stack has size 2 
doRot :: EDState -> EDState
--Pattern matches small stack cases.
doRot EDState{stack = [], fns = fs, vars = vs} = 
    EDState{stack = [], fns = fs, vars = vs}
doRot EDState{stack = [x], fns = fs, vars = vs} = 
    EDState{stack = [x], fns = fs, vars = vs}
doRot EDState{stack = [x, y], fns = fs, vars = vs} = 
    EDState{stack = [x, y], fns = fs, vars = vs}

--Peforms rotation if stack is 3 or greater in size.
doRot state =
    let (popState, c, b, a) = fsPop3 state
        statePush1 = fsPush a popState
        statePush2 = fsPush c statePush1
    in fsPush b statePush2   

-- duplicate the top value on the stack. 1 -> 1 1 
doDup :: EDState -> EDState

--Nothing to duplicate if stack is empty.
doDup EDState{stack = [], fns = fs, vars = vs} =
    EDState{stack = [], fns = fs, vars = vs}

--Duplicates top of stack.
doDup state = 
    let (newState, top) = fsPop state
        push1 = fsPush top newState
    in fsPush top push1 

--Tests equality. Pushes 1 if equal, 0 if not equal.
doEqual :: EDState -> EDState
doEqual EDState{stack = [], fns = fs, vars = vs} = 
    error "Eqality comparison requires two operands!"
doEqual EDState{stack = [x], fns = fs, vars = vs} = 
    error "Eqality comparison requires two operands!"
doEqual state = 
    let (state', secondToTop, top) = fsPop2 state
    in if (top == secondToTop) 
            then fsPush (Boolean True) state' 
            else fsPush (Boolean False) state'

--Tests inequality. Pushes 1 if not equal, 0 if equal.

doNotEqual :: EDState -> EDState
doNotEqual EDState{stack = [], fns = fs, vars = vs} = 
    error "Ineqality comparison requires two operands!"
doNotEqual EDState{stack = [x], fns = fs, vars = vs} = 
    error "Ineqality comparison requires two operands!"
doNotEqual state = 
    let (state', secondToTop, top) = fsPop2 state
    in if (top /= secondToTop) 
            then fsPush (Boolean True) state' 
            else fsPush (Boolean False) state'

--Tests if second to top of stack is greater than top of stack. 
-- Pushes 1 if true, 0 if false.
doGreaterThan :: EDState -> EDState
doGreaterThan EDState{stack = [], fns = fs, vars = vs} = 
    error "Greater than comparison requires two operands!"
doGreaterThan EDState{stack = [x], fns = fs, vars = vs} = 
    error "Greater than comparison requires two operands!"
doGreaterThan state = 
    let (state', secondToTop, top) = fsPop2 state
    in if (secondToTop > top) 
            then fsPush (Boolean True) state' 
            else fsPush (Boolean False) state'

--Tests if second to top of stack is less than top of stack. 
-- Pushes 1 if true, 0 if false.
doLessThan :: EDState -> EDState
doLessThan EDState{stack = [], fns = fs, vars = vs} = 
    error "Less than comparison requires two operands!"
doLessThan EDState{stack = [x], fns = fs, vars = vs} = 
    error "Less than comparison requires two operands!"
doLessThan state = 
    let (state', secondToTop, top) = fsPop2 state
    in if (secondToTop < top) 
            then fsPush (Boolean True) state' 
            else fsPush (Boolean False) state'

--Tests if second to top of stack is greater than equal to top of stack. 
-- Pushes 1 if true, 0 if false.
doGreaterThanEqualTo :: EDState -> EDState
doGreaterThanEqualTo EDState{stack = [], fns = fs, vars = vs} = 
    error "Greater than equal to comparison requires two operands!"
doGreaterThanEqualTo EDState{stack = [x], fns = fs, vars = vs} = 
    error "Greater than equal to comparison requires two operands!"
doGreaterThanEqualTo state = 
    let (state', secondToTop, top) = fsPop2 state
    in if (secondToTop >= top) 
            then fsPush (Boolean True) state' 
            else fsPush (Boolean False) state'

--Tests if second to top of stack is less than equal to top of stack. 
-- Pushes 1 if true, 0 if false.
doLessThanEqualTo :: EDState -> EDState
doLessThanEqualTo EDState{stack = [], fns = fs, vars = vs} = 
    error "Less than equal to comparison requires two operands!"
doLessThanEqualTo EDState{stack = [x], fns = fs, vars = vs} = 
    error "Less than equal to comparison requires two operands!"
doLessThanEqualTo state = 
    let (state', secondToTop, top) = fsPop2 state
    in if (secondToTop <= top) 
            then fsPush (Boolean True) state' 
            else fsPush (Boolean False) state'

--Performs logical AND function on top two elements of stack.
doAnd :: EDState -> EDState
doAnd EDState{stack = [], fns = fs, vars = vs} = 
    error "Logical AND operation requires two operands!"
doAnd EDState{stack = [x], fns = fs, vars = vs} = 
    error "Logical AND operation requires two operands!"
doAnd state = 
    let (state', secondToTop, top) = fsPop2 state
    in doAnd' state' top secondToTop

--Performs logical AND function on two operands and returns an updated EDState.
-- On failure, an error is thrown.
doAnd' :: EDState -> Value -> Value -> EDState
doAnd' state (Boolean False) (Boolean False) = fsPush (Boolean False) state
doAnd' state (Boolean False) (Boolean True) = fsPush (Boolean False) state
doAnd' state (Boolean True) (Boolean False) = fsPush (Boolean False) state
doAnd' state (Boolean True) (Boolean True) = fsPush (Boolean True) state
doAnd' _ _ _ = error "Operator (and) error. Logical AND requires two boolean types."

--Performs logical OR function on top two elements of stack.
doOr :: EDState -> EDState
doOr EDState{stack = [], fns = fs, vars = vs} = 
    error "Logical OR operation requires two operands!"
doOr EDState{stack = [x], fns = fs, vars = vs} = 
    error "Logical OR operation requires two operands!"
doOr state = 
    let (state', secondToTop, top) = fsPop2 state
    in doOr' state' top secondToTop

--Performs logical OR function on two operands and returns an updated EDState.
-- On failure, an error is thrown.
doOr' :: EDState -> Value -> Value -> EDState
doOr' state (Boolean False) (Boolean False) = fsPush (Boolean False) state
doOr' state (Boolean False) (Boolean True) = fsPush (Boolean True) state
doOr' state (Boolean True) (Boolean False) = fsPush (Boolean True) state
doOr' state (Boolean True) (Boolean True) = fsPush (Boolean True) state
doOr' _ _ _ = error "Operator (or) error. Logical OR requires two boolean types."

--Performs logical XOR function on top two elements of stack.
doXor :: EDState -> EDState
doXor EDState{stack = [], fns = fs, vars = vs} = 
    error "Logical XOR operation requires two operands!"
doXor EDState{stack = [x], fns = fs, vars = vs} = 
    error "Logical XOR operation requires two operands!"
doXor state = 
    let (state', secondToTop, top) = fsPop2 state
    in doXor' state' top secondToTop

--Performs logical XOR function on two operands and returns an updated EDState.
-- On failure, an error is thrown.
doXor' :: EDState -> Value -> Value -> EDState
doXor' state (Boolean False) (Boolean False) = fsPush (Boolean False) state
doXor' state (Boolean False) (Boolean True) = fsPush (Boolean True) state
doXor' state (Boolean True) (Boolean False) = fsPush (Boolean True) state
doXor' state (Boolean True) (Boolean True) = fsPush (Boolean False) state
doXor' _ _ _ = error "Operator (xor) error. Logical XOR requires two boolean types."

--Performs the logical NOT operator on given boolean 
-- and throws errors when things go wrong.
doNot :: EDState -> EDState
doNot EDState{stack = [], fns = fs, vars = vs} = 
    error "Logical NOT operation requires one operand!"
doNot state = 
    let (state', top) = fsPop state
    in doNot' state' top

--Performs negation if input value is of type boolean.
doNot' :: EDState -> Value -> EDState
doNot' state (Boolean False) = fsPush (Boolean True) state
doNot' state (Boolean True) = fsPush (Boolean False) state
doNot' _ _ = error "Operator (not) error. Logical NOT requires one boolean type operand."

--Pushes an item into a list.
doPush :: EDState -> EDState
doPush EDState{stack = [], fns = fs, vars = vs} =
    error "List push operation requires two operands!"
doPush EDState{stack = [x], fns = fs, vars = vs} =
    error "List push operation requires two operands!"
doPush state = 
    let (state', list, val) = fsPop2 state
    in doPush' state' list val

--Pushes item to list or string.
doPush' :: EDState -> Value -> Value -> EDState
doPush' state (List ls) valToPush = fsPush (List (ls ++ [valToPush])) state
doPush' state (String st) (Char c) = fsPush (String (st ++ [c])) state
doPush' _ _ _ = error "Operator (push) error. Push operator needs a list or string and a value or char to be pushed."

--Pops an item from the list or string and pushes it to the stack.
doPop :: EDState -> EDState
doPop EDState{stack = [], fns = fs, vars = vs} =
    error "pop operation needs an operand!"
doPop state = 
    let (state', list) = fsPop state
    in doPop' state' list

--Pops item from list and pushes it to stack.
doPop' :: EDState -> Value -> EDState
--Nothing happens if list is empty.
doPop' state (List []) = fsPush (List []) state
doPop' state (List ls) = 
    let state' = fsPush (List $ init ls) state
    in fsPush (last ls) state'
doPop' state (String st) = 
    let state' = fsPush (String $ init st) state
    in fsPush (Char $ last st) state'
doPop' _ _ = error "Operator (pop) error. Pop operator needs a list or string to pop items from."

--Pushes an item into a list from the front.
doFpush :: EDState -> EDState
doFpush EDState{stack = [], fns = fs, vars = vs} =
    error "fpush operation requires two operands!"
doFpush EDState{stack = [x], fns = fs, vars = vs} =
    error "fpush operation requires two operands!"
doFpush state = 
    let (state', list, val) = fsPop2 state
    in doFpush' state' list val

--Pushes item to list.
doFpush' :: EDState -> Value -> Value -> EDState
doFpush' state (List ls) valToPush = fsPush (List (valToPush : ls)) state
doFpush' state (String st) (Char c) = fsPush (String (c : st)) state
doFpush' _ _ _ = error "Operator (fpush) error. Operator fpush needs a list/string and a value/char to be pushed to front."

--Pops an item from the front of the list and pushes it to the stack.
doFpop :: EDState -> EDState
doFpop EDState{stack = [], fns = fs, vars = vs} =
    error "List fpop operation needs an operand!"
doFpop state = 
    let (state', list) = fsPop state
    in doFpop' state' list

--Pops item from list and pushes it to stack.
doFpop' :: EDState -> Value -> EDState
--Nothing happens if list is empty.
doFpop' state (List []) = fsPush (List []) state
doFpop' state (List ls) = 
    let state' = fsPush (List $ tail ls) state
    in fsPush (head ls) state'
--String case.
doFpop' state (String st) = 
    let state' = fsPush (String $ tail st) state
    in fsPush (Char $ head st) state'
doFpop' _ _ = error "Operator (fpop) error. Pop operator needs a list to pop items from."

--Finds item in list of input index.
doIndex :: EDState -> EDState
doIndex EDState{stack = [], fns = fs, vars = vs} =
    error "index operation requires two operands!"
doIndex EDState{stack = [x], fns = fs, vars = vs} =
    error "index operation requires two operands!"
doIndex state = 
    let (state', list, index) = fsPop2 state
    in doIndex' state' list index

--Retrieves item at index in list or string.
doIndex' :: EDState -> Value -> Value -> EDState
doIndex' state (List []) _ = error "Can't index into empty list."
doIndex' state (String "") _ = error "Can't index into empty string."
doIndex' state (List ls) (Integer index) = 
    let state' = fsPush (List ls) state
    in fsPush (ls !! index) state'
--String case.
doIndex' state (String st) (Integer index) = 
    let state' = fsPush (String st) state
    in fsPush (Char $ st !! index) state'
doIndex' _ _ _ = error "Operator (index) error. Index operator needs a list/string and an index value. \n Index must use type Integer to perform an index"

--Takes the length of a list or string at the top 
-- of the stack and pushes resulting length to top of stack.
doLength :: EDState -> EDState
doLength EDState{stack = [], fns = fs, vars = vs} =
    error "Length operation requires one operand!"
doLength state = doLength' state (fsTop state)

--Performs actual length function.
doLength' :: EDState -> Value -> EDState
doLength' state (List ls) = fsPush (Integer $ length ls) state
doLength' state (String st) = fsPush (Integer $ length st) state
doLength' state _ = error "Operator (length) error. List or string type is needed for length function to work."

--Determines if the list or string at the top
-- of the stack is empty or not.
doIsEmpty :: EDState -> EDState
doIsEmpty EDState{stack = [], fns = fs, vars = vs} =
    error "isEmpty operation requires one operand!"
doIsEmpty state = doIsEmpty' state (fsTop state)

--Performs actual length function.
doIsEmpty' :: EDState -> Value -> EDState
doIsEmpty' state (List ls) = fsPush (Boolean $ null ls) state
doIsEmpty' state (String st) = fsPush (Boolean $ null st) state
doIsEmpty' state _ = error "Operator (isEmpty) error. List or string type is needed to test for emptyness."

--Sets the string or list at the top of the stack to empty.
doClear :: EDState -> EDState
doClear EDState{stack = [], fns = fs, vars = vs} =
    error "Clear operation requires one operand!"
doClear state = 
    let (state', list) = fsPop state
    in doClear' state' list

--Performs clear operation.
doClear' :: EDState -> Value -> EDState
doClear' state (List ls) = fsPush (List []) state
doClear' state (String st) = fsPush (String "") state
doClear' state _ = error "Operator (clear) error. List or string is needed for clear to occur."

--Used to turn a value of one type into another.
doCast :: EDState -> EDState
doCast EDState{stack = [], fns = _, vars = _} =
    error "Operation cast requires two operands!"
doCast EDState{stack = [x], fns = _, vars = _} =
    error "Operation cast requires two operands!"
doCast state =
    let (state', val, castType) = fsPop2 state
    in doCast' state' val castType

--Performs the actual cast operation.
doCast' :: EDState -> Value -> Value -> EDState

doCast' state (Boolean b) (String "Integer") = fsPush (Integer (if b then 1 else 0)) state
doCast' state (Boolean b) (String "BigInteger") = fsPush (BigInteger (if b then 1 else 0)) state
doCast' state (Boolean b) (String "String") = fsPush (String $ show b) state
doCast' state (Boolean b) (String "Boolean") = fsPush (Boolean b) state --Do nothing case.

doCast' state (BigInteger n) (String "String") = fsPush (String $ show n) state
doCast' state (BigInteger n) (String "Integer") = fsPush (Integer (fromIntegral n :: Int)) state
doCast' state (BigInteger n) (String "BigInteger") = fsPush (BigInteger n) state --Do nothing case.
doCast' state (BigInteger n) (String "Float") = fsPush (Float (fromIntegral n :: Float)) state
doCast' state (BigInteger n) (String "Double") = fsPush (Double (fromIntegral n :: Double)) state

doCast' state (Integer n) (String "String") = fsPush (String $ show n) state
doCast' state (Integer n) (String "Integer") = fsPush (Integer n) state --Do nothing case
doCast' state (Integer n) (String "BigInteger") = fsPush (BigInteger (fromIntegral n :: Integer)) state 
doCast' state (Integer n) (String "Float") = fsPush (Float (fromIntegral n :: Float)) state
doCast' state (Integer n) (String "Double") = fsPush (Double (fromIntegral n :: Double)) state

doCast' state (Float n) (String "String") = fsPush (String $ show n) state
doCast' state (Float n) (String "Integer") = fsPush (Integer (truncate n)) state
doCast' state (Float n) (String "BigInteger") = fsPush (BigInteger (floor n :: Integer)) state 
doCast' state (Float n) (String "Float") = fsPush (Float n) state --Do nothing case.
doCast' state (Float n) (String "Double") = fsPush (Double (realToFrac n :: Double)) state

doCast' state (Double n) (String "String") = fsPush (String $ show n) state
doCast' state (Double n) (String "Integer") = fsPush (Integer (truncate n)) state
doCast' state (Double n) (String "BigInteger") = fsPush (BigInteger (floor n :: Integer)) state 
doCast' state (Double n) (String "Float") = fsPush (Float (realToFrac n :: Float)) state
doCast' state (Double n) (String "Double") = fsPush (Double n) state --Do nothing case.

-- doCast' state (String s) (String "String") = fsPush (String s) state --Do nothing case.
-- doCast' state (String s) (String "Integer") = fsPush (Integer (fromIntegral n :: Int)) state
-- doCast' state (String s) (String "BigInteger") = fsPush (BigInteger (fromIntegral n :: Integer)) state 
-- doCast' state (String s) (String "Float") = fsPush (Float n) state
-- doCast' state (String s) (String "Double") = fsPush (Double (fromIntegral n :: Double)) state

doCast' state val _ = error "Operator (cast) error. Second argument of cast needs to be string."


-- performs the operation identified by the string. for example, doOp state "+"
-- will perform the "+" operation, meaning that it will pop two values, sum them,
-- and push the result. 
doOp :: String -> EDState -> EDState
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
--Type stuff
doOp "cast" = doCast

-- Error thrown if reached here.
doOp op = error $ "unrecognized word: " ++ op 

astNodeToString :: AstNode -> String
astNodeToString (Terminal (Word w)) = w

makeVar :: EDState -> String -> EDState
makeVar state varName = 
    let lkup = M.lookup varName (vars state)
    --Throw error if variable exists. Otherwise, make variable by inserting it into hash table.
    in case lkup of
        Just _ -> error "Variable Mak Error: Variable already exists."
        Nothing -> let vars' = M.insert varName (fsTop state) (vars state)
            in EDState{stack = (stack state), fns = (fns state), vars = vars'}

--Comapres types in order to enforce static typing when mutating variables.
compareTypesForMut :: Value -> Value -> Bool
compareTypesForMut (Boolean _) (Boolean _) = True
compareTypesForMut (BigInteger _) (BigInteger _) = True
compareTypesForMut (Integer _) (Integer _) = True
compareTypesForMut (Double _) (Double _) = True
compareTypesForMut (Float _) (Float _) = True
compareTypesForMut (String _) (String _) = True
compareTypesForMut (Char _) (Char _) = True
compareTypesForMut (List _) (List _) = True
compareTypesForMut _ _ = False

mutateVar :: EDState -> String -> EDState
mutateVar state varName =
    let lkupVal = M.lookup varName (vars state)
        newVal = fsTop state
    --If variable exists it can be mutated. Otherwise, an error is thrown.
    in case lkupVal of
        Just value -> if compareTypesForMut value newVal then 
            let vars' = M.insert varName (fsTop state) (vars state)
            in EDState{stack = (stack state), fns = (fns state), vars = vars'}
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

-- execute an AstNode
doNode :: AstNode -> EDState -> EDState

-- Runs true branch if top of stack is true 
--and false branch if top of stack is false.
doNode If { ifTrue = trueBranch, ifFalse = falseBranch } state = 
    let top = if (null (stack state)) 
        then error "If statement error: \nNo boolean value for if to check because stack is empty." 
        else fsTop state
    in 
    if (top == (Boolean True)) then 
        doNode trueBranch state 
    else doNode falseBranch state

--Patterm matches function definition.
doNode (Expression((Function {funcCmd = cmd, funcName = name, funcBod = body}):rest)) state =
    case (astNodeToString cmd) of 
        "def" -> doNode (Expression(rest)) (funcDef state (astNodeToString name) body)
        "call" -> let (state', funcBod) = funcCall state (astNodeToString name)
                      state'' = doNode funcBod state'
                  in doNode (Expression(rest)) state''
        _ -> error "Function Error: Invalid function command given. Valid: def, call"

--Runs all the different cases of variable actions.
doNode (Expression((Variable{varName = name, varCmd = cmd}):rest)) state =
    case (astNodeToString cmd) of
        "mak" -> if null (stack state) 
            then error "Variable Mak Error: Can't create variable when stack is empty."
            else
                doNode (Expression rest) (makeVar state (astNodeToString name))
                           
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

        "mut" -> if null (stack state) 
            then error "Variable Mut Error: Can't mutate variable when stack is empty."
            else
                doNode (Expression rest) (mutateVar state (astNodeToString name))

        _ -> error "Variable Command Error: Invalid variable command given. Valid: mak, get, mut, del"

--Runs while loop.
doNode ( While loopBody ) state =
    let top = if (null (stack state)) 
        then error "While Loop error: \nNo boolean value for while loop to check because stack is empty." 
        else fsTop state

        --Creates new stack if loop body runs.
        -- Otherwise newState is same as state.
        newState = if top == (Boolean True) then
            doNode (loopBody) state
        else state

    --If loop ran and can run again, it's run again, 
    -- otherwise, state is returned.
        newTop = fsTop newState
    in if (newTop == (Boolean True)) then doNode ( While loopBody ) newState else newState

-- doing a terminal changes depending on whether it's a word or a number. 
-- if it's a number, push it...
doNode ( Terminal ( Val v ) ) state = fsPush v state

-- ...if it's a word, execute the operation
doNode ( Terminal ( Word o ) ) state = doOp o state

-- "doing" an empty expression does nothing
doNode ( Expression [] ) state = state

-- "doing" a non-empty expression tries to execute every node in the expression
doNode ( Expression ( first:rest ) ) state =  
    let stateAfterFirst = doNode first state
    in doNode (Expression (rest)) stateAfterFirst                                     

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
fsNew :: EDState
fsNew = EDState { stack = [], fns = M.empty, vars = M.empty }

-- push a new value onto the stack
fsPush :: Value -> EDState -> EDState
fsPush val state = EDState { stack = val : stack state, fns = (fns state), vars = (vars state)}

-- remove a value from the stack, or print an error if nothing is there.
-- returns the value removed and the new state 
fsPop :: EDState -> ( EDState, Value )
fsPop state = 
    let top = head $ stack state 
        new_stack = tail $ stack state  
    in  
        ( EDState { stack = new_stack, fns = (fns state), vars = (vars state) }, top )

-- remove two values from the stack. return the new stack and the two items.
fsPop2 :: EDState -> ( EDState, Value, Value )
fsPop2 state = 
    let top  = head $ stack state
        secondToTop = head (tail $ stack state)
        stackNew = tail (tail (stack state))
    in
        (EDState {stack = stackNew, fns = (fns state), vars = (vars state)}, secondToTop, top)

-- remove three values from the stack. return the new stack and the three items. 
fsPop3 :: EDState -> ( EDState, Value, Value, Value )
fsPop3 state = 
    let top = fsTop state
        secondToTop = fsTop EDState{ stack = (tail $ stack state), 
            fns = (fns state), vars = (vars state) }
        thirdToTop = fsTop EDState{ stack = (tail $ tail $ stack state), 
            fns = (fns state), vars = (vars state)}
        stackNew = tail $ tail $ tail $ stack state
    in
        (EDState {stack = stackNew, fns = (fns state), vars = (vars state)}, 
            thirdToTop, secondToTop, top) 

-- return the value on top of the stack 
fsTop :: EDState -> Value 
fsTop state = head $ stack state 

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
    | t == "[]" = Val $ List []  --Empty list case.
    | (head t) == '"' && (last t) == '"' = Val $ String (read t :: String)  --String case                                                                  
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
printStack (x:xs) = print x >> printStack xs

main :: IO ()
main = do
    -- get all the code passed to STDIN as a giant string 
    code <- getContents

    -- convert it into a list of tokens
    let tokens = removeComments False [] ( tokenize code )

    -- parse the ast 
    let ast = parseExpression tokens

    --print ast

    printStack $ reverse $ stack $ doNode ast fsNew 
