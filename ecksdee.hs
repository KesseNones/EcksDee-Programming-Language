--Jesse A. Jones
--Version: 2024-05-14.13
--Toy Programming Language Named EcksDee

{-
    ISSUES:
        -Extra error checking for casting is a good idea.
        -Maybe have errors show line number 
            of code file where error happened, somehow. 
            It would make user debugging much less ass.
        -Standardize errors.
        -Casting edge case may exist where BigInteger to Char works when it shouldn't! (Fixed? Needs more testing.)
-}

import Data.List
import Data.Char
import Data.Maybe 
import Debug.Trace
import Text.Read (readMaybe)
import System.IO
import System.Environment
import qualified Data.Map.Strict as M
import Control.DeepSeq
import Control.Exception
import Data.Typeable

data Value =                                         
        BigInteger Integer
    |   Integer Int
    |   Float Float
    |   Double Double
    |   String {chrs :: [Char], len :: Int}
    |   Char Char
    |   Boolean Bool
    |   List { items :: M.Map Int Value, len :: Int}
    |   Object { fields :: M.Map String Value }
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

    |   LocVar {name :: AstNode, cmd :: AstNode}

    |   AttErr {attempt :: AstNode, onError :: AstNode}

    deriving ( Show )

-- This is the state of the interpreter. 
-- Currently it stores the stack, which is where all of the data lives. 
data EDState = EDState { 
    stack :: [Value], 
    fns :: M.Map String AstNode,
    vars :: M.Map String Value,
    frames :: [M.Map String Value]
}

data GeneralException = GeneralException String deriving (Show, Typeable)

instance Exception GeneralException

--Used in throwing error.
throwError :: String -> EDState -> IO EDState
throwError msg = throw $ GeneralException msg

findTypeStrsForError :: Value -> Value -> (String, String)
findTypeStrsForError x y = (chrs $ doQueryType' x, chrs $ doQueryType' y)

--Adds two values together. If the types can't be added, throw an error.
--Can also act as an OR operator for Boolean type.
addVals :: Value -> Value -> Either Value String 
addVals (BigInteger a) (BigInteger b) = Left $ BigInteger (a + b)
addVals (Integer a) (Integer b) = Left $ Integer (a + b)
addVals (Double a) (Double b) = Left $ Double (a + b)
addVals (Float a) (Float b) = Left $ Float (a + b)
addVals (Boolean a) (Boolean b) = Left $ Boolean (a || b)
addVals a b =
    let (aType, bType) = findTypeStrsForError a b  
    in Right ("Operator (+) error. Can't add types together that are not both types of BigIntegers, Integers, Floats, Doubles, or Booleans! " 
        ++ "Attempted types were: " 
        ++ aType ++ " and " ++ bType)

-- Subtracts two values. If the types can't be subtracted, throw an error.
subVals :: Value -> Value -> Either Value String
subVals (BigInteger a) (BigInteger b) = Left $ BigInteger (b - a)
subVals (Integer a) (Integer b) = Left $ Integer (b - a)
subVals (Double a) (Double b) = Left $ Double (b - a)
subVals (Float a) (Float b) = Left $ Float (b - a)
subVals a b =
    let (bType, aType) = findTypeStrsForError b a  
    in Right ("Operator (-) error. Can't subtract types that are not both types of BigIntegers, Integers, Floats, or Doubles! " 
        ++ "Attempted types were: " 
        ++ bType ++ " and " ++ aType)

--Multiplies two values together. If the types can't be multiplied, throw an error.
multVals :: Value -> Value -> Either Value String
multVals (BigInteger a) (BigInteger b) = Left $ BigInteger (a * b)
multVals (Integer a) (Integer b) = Left $ Integer (a * b)
multVals (Double a) (Double b) = Left $ Double (a * b)
multVals (Float a) (Float b) = Left $ Float (a * b)
multVals (Boolean a) (Boolean b) = Left $ Boolean (a && b)
multVals a b =
    let (aType, bType) = findTypeStrsForError a b  
    in Right ("Operator (*) error. Can't multiply types that are not both types of BigIntegers, Integers, Floats, Doubles, or Booleans! " 
        ++ "Attempted types were: " 
        ++ aType ++ " and " ++ bType)

-- Divides two values. If the types can't be divided, throw an error.
divideVals :: Value -> Value -> Value
divideVals (BigInteger a) (BigInteger b) = if (a /= 0) then BigInteger (b `div` a) else error "Operator (/) error. Can't divide by zero for type BigInteger!"
divideVals (Integer a) (Integer b) = if (a /= 0) then Integer (b `div` a) else error "Operator (/) error. Can't divide by zero for type Integer!"
divideVals (Double a) (Double b) = if (a /= 0.0) then Double (b / a) else error "Operator (/) error. Can't divide by zero for type Double!"
divideVals (Float a) (Float b) = if (a /= 0.0) then Float (b / a) else error "Operator (/) error. Can't divide by zero for type Float!"
divideVals _ _ = error "Operator (/) error. \n Can't divide types that aren't BigIntegers, Integers, or Floats. \n Data types also need to match"

--Performs modulo operation on two values.
modVals :: Value -> Value -> Value
modVals (BigInteger a) (BigInteger b) = BigInteger (b `mod` a)
modVals (Integer a) (Integer b) = Integer (b `mod` a)
modVals _ _ = error "Operator (%) error. \n Can't perform modulo on types that aren't BigIntegers or Integers. \n Data types also have to match."

doConcat'' :: Value -> Value -> Int -> Int -> Value
doConcat'' List{items = is, len = l} List{items = accs, len = accLen} index offset 
    |   (index < l) = 
            let ins = case (M.lookup index is) of 
                    Just i -> i 
                    Nothing -> error "SHOULD NEVER GET HERE!!!"
                accs' = M.insert (index + offset) ins accs
            in doConcat'' List{items = is, len = l} List{items = accs', len = accLen + 1} (index + 1) offset  
    |   otherwise = List{items = accs, len = accLen}

--Concatenates two Strings or Lists.
doConcat' :: Value -> Value -> Value
doConcat' (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = String {chrs = acs ++ bcs, len = al + bl}
doConcat' List {items = as, len = al} List {items = bs, len = bl} = 
    let List{items = cs, len = cl} = doConcat'' List{items = as, len = al} List{items = M.empty, len = 0} 0 0
        List{items = ds, len = dl} = doConcat'' List{items = bs, len = bl} List{items = cs, len = cl} 0 al
    in List{items = ds, len = dl}
doConcat' _ _ = error "Operator (++) error. \n Can't perform concatenation on types that aren't Strings or Lists.\n Types also have to both be strings or lists!"

--Concatentates two Strings/Lists together.
doConcat :: EDState -> IO EDState
doConcat state = do 
    let stck = (stack state)
    case stck of 
        [] -> throwError "Operator (++) error. Concatenation requires two operands!" state 
        [x] -> throwError "Operator (++) error. Concatenation requires two operands!" state
        vals -> 
            let (state', a, b) = fsPop2 state
                ret = (\x -> return (fsPush x state'))
            in ret $! (doConcat' a b)

--Adds two values on stack if they can be added.
doAdd :: EDState -> IO EDState
doAdd state = 
    let stck = (stack state)
    in case stck of 
        [] -> throwError "Operator (+) error. Addition requires two operands; none provided!" state
        [x] -> throwError "Operator (+) error. Addition requires two operands; only one provided!" state
        vals ->  
            let (state', a, b) = fsPop2 state
                addRes = addVals a b
            in case addRes of 
                Left v -> return (fsPush v state')
                Right err -> throwError err state'

--Subtracts two values on stack if they can be subtracted.
doSub :: EDState -> IO EDState
doSub state = 
    case (stack state) of 
        [] -> throwError "Operator (-) error. Subtraction requires two operands; none provided!" state
        [x] -> throwError "Operator (-) error. Subtraction requires two operands; only one provided!" state
        vals ->  
            let (state', b, a) = fsPop2 state
                subRes = subVals a b
            in case subRes of
                Left v -> return (fsPush v state')
                Right err -> throwError err state'

--Multiplies two values on top of stack if they can be multiplied.
doMul :: EDState -> IO EDState
doMul state =  
    case (stack state) of 
        [] -> throwError "Operator (*) error. Multiplication requires two operands; none provided!" state
        [x] -> throwError "Operator (*) error. Multiplication requires two operands; only one provided!" state
        vals -> 
            let (state', a, b) = fsPop2 state
                mulRes = multVals a b
            in case mulRes of 
                Left v -> return (fsPush v state')
                Right err -> throwError err state'

--Divides two values on top of stack if they can be divided.
--Errors out if problem happens.
doDiv :: EDState -> IO EDState
doDiv state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (/) error. Division requires two operands!"
        [x] -> error "Operator (/) error. Division requires two operands!"
        vals -> 
            let (state', b, a) = fsPop2 state
                ret = (\x -> return (fsPush x state'))
            in ret $! (divideVals a b)

--Mods two values on top of stack if they can be modded.
--Errors out if problem happens.
doModulo :: EDState -> IO EDState
doModulo state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (%) error. Modulo requires two operands!"
        [x] -> error "Operator (%) error. Modulo requires two operands!"
        vals -> 
            let (state', b, a) = fsPop2 state
                ret = (\x -> return (fsPush x state'))
            in ret $! (modVals a b)

--Swaps the top two values at the top of the stack.
doSwap :: EDState -> IO EDState
doSwap state = do 
    let stck = (stack state)
    case stck of 
        [] -> return ( EDState{stack = [], fns = (fns state), vars = (vars state), frames = (frames state)} )
        [x] -> return ( EDState{stack = [x], fns = (fns state), vars = (vars state), frames = (frames state)} )
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

--Clears the entire stack to empty. 
-- Avoids having to type drop over and over again.
doDropStack :: EDState -> IO EDState
doDropStack EDState{stack = _, fns = fs, vars = vs, frames = fms} = return (EDState{stack = [], fns = fs, vars = vs, frames = fms})

--Rotates the top values on the stack.
--If there's 0 or 1 items, nothing happens.
--2 Items is identical to swap.
--3 items performs the rotation.
doRot :: EDState -> IO EDState
doRot state = do 
    let stck = (stack state)
    case stck of 
        [] -> return ( EDState{stack = [], fns = (fns state), vars = (vars state), frames = (frames state)} )
        [x] -> return ( EDState{stack = [x], fns = (fns state), vars = (vars state), frames = (frames state)} )
        [x, y] -> return ( EDState{stack = [y, x], fns = (fns state), vars = (vars state), frames = (frames state)} )
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
        [] -> return ( EDState{stack = [], fns = (fns state), vars = (vars state), frames = (frames state)} )
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
        vals -> 
            let (state', b, a) = fsPop2 state
                ret = (\eqRes -> return (fsPush eqRes state'))
            in ret $! (doEqual' a b)

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
        vals ->  
            let (state', b, a) = fsPop2 state
                ret = (\x -> return (fsPush x state'))
            in ret $! (doNotEqual' a b) 

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
        vals -> 
            let (state', b, a) = fsPop2 state
                ret = (\x -> return (fsPush x state'))
            in ret $! (doGreaterThan' b a)
                
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
        vals -> 
            let (state', b, a) = fsPop2 state
                ret = (\x -> return (fsPush x state'))
            in ret $! (doLessThan' b a) 

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
        vals -> 
            let (state', b, a) = fsPop2 state
                ret = (\x -> return (fsPush x state'))
            in ret $! (doGreaterThanEqualTo' b a) 

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
        vals -> 
            let (state', b, a) = fsPop2 state
                ret = (\x -> return (fsPush x state'))
            in ret $! (doLessThanEqualTo' b a)

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
        vals -> 
            let (state', b, a) = fsPop2 state 
                ret = (\x -> return x)
            in ret $! (doAnd' state' a b)

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
        vals -> 
            let (state', b, a) = fsPop2 state
                ret = (\x -> return x)
            in ret $! (doOr' state' a b)  

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
        vals -> 
            let (state', b, a) = fsPop2 state 
                ret = (\x -> return x)
            in ret $! (doXor' state' a b) 

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
        vals -> 
            let (state', top) = fsPop state
                ret = (\x -> return x)
            in ret $! (doNot' state' top) 

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
        vals ->  
            let (state', list, val) = fsPop2 state
                ret = (\x -> return x)
            in ret $! (doPush' state' list val)  

--Pushes item to list or string.
doPush' :: EDState -> Value -> Value -> EDState
doPush' state (List {items = is, len = l}) valToPush = fsPush ( List {items = (M.insert l valToPush is), len = l + 1} ) state
doPush' state (String {chrs = cs, len = l}) (Char c) = fsPush (String {chrs = cs ++ [c], len = l + 1}) state
doPush' _ _ _ = error "Operator (push) error. Push operator needs a list or string and a value or char to be pushed!"

--Pops an item from the list or string and pushes it to the stack.
doPop :: EDState -> IO EDState
doPop state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (pop) error. Pop operator needs a list to pop from!"
        vals -> 
            let (state', list) = fsPop state
                ret = (\x -> return x)
            in ret $! (doPop' state' list) 

--Pops item from list and pushes it to stack.
doPop' :: EDState -> Value -> EDState
--Nothing happens if list is empty.
doPop' state (List {items = is, len = 0}) = fsPush (List {items = is, len = 0}) state
doPop' state (List {items = is, len = l}) = 
    let popped = case (M.lookup (l - 1) is) of 
            Just i -> i 
            Nothing -> error "Should never happen!!!"
        newLs = M.delete (l - 1) is
        state' = fsPush( List {items = newLs, len = l - 1}) state
    in fsPush popped state'
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
        vals -> 
            let (state', list, val) = fsPop2 state
                ret = (\x -> return x)
            in ret $! (doFpush' state' list val)  

--Pushes item to list front.
doFpush' :: EDState -> Value -> Value -> EDState
doFpush' state (List {items = is, len = l}) valToPush = fsPush ( doFpush'' List {items = is, len = l} 0 valToPush ) state
doFpush' state (String {chrs = cs, len = l}) (Char c) = fsPush (String {chrs = (c : cs), len = l + 1}) state
doFpush' _ _ _ = error "Operator (fpush) error. Operator fpush needs a list/string and a value/char to be pushed to front."

doFpush'' :: Value -> Int -> Value -> Value
doFpush'' List{items = is, len = l} index insVal 
    |    index < l = 
            let old = case (M.lookup index is) of
                    Just i -> i 
                    Nothing -> error "Shouldn't ever get here!!!"
                is' = M.insert index insVal is
            in doFpush'' List{items = is', len = l} (index + 1) old
    |    otherwise = 
            let is' = M.insert index insVal is 
            in List{items = is', len = l + 1}

--Pops an item from the front of the list and pushes it to the stack.
doFpop :: EDState -> IO EDState
doFpop state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (fpop) error. List needed!"
        vals -> 
            let (state', list) = fsPop state
                ret = (\x -> return x)
            in ret $! (doFpop' state' list)

--Pops item from list and pushes it to stack.
doFpop' :: EDState -> Value -> EDState
--Nothing happens if list is empty.
doFpop' state (List {items = is, len = 0}) = fsPush (List {items = is, len = 0}) state
doFpop' state (List {items = is, len = l}) = 
    let popped = case (M.lookup 0 is) of 
            Just i -> i 
            Nothing -> error "Should NEVER get here!!!"
        List{items = newLs, len = newLen} = doFpop'' List{items = is, len = l} List{items = M.empty, len = 0} 1
        state' = fsPush (List{items = newLs, len = newLen}) state
    in fsPush (popped) state'

--String case.
doFpop' state (String {chrs = "", len = 0}) = fsPush (String {chrs = "", len = 0}) state
doFpop' state (String {chrs = cs, len = l}) = 
    let state' = fsPush (String {chrs = tail cs, len = l - 1}) state
    in fsPush (Char $ head cs) state'
doFpop' _ _ = error "Operator (fpop) error. Pop operator needs a list to pop items from."

--Builds new list after pop.
doFpop'' :: Value -> Value -> Int -> Value
doFpop'' List{items = as, len = al} List{items = bs, len = bl} index 
    |   index < al = 
            let ins = case (M.lookup index as) of 
                    Just i -> i 
                    Nothing -> error "Shouldn't EVER get here!"
                bs' = M.insert (index - 1) ins bs 
            in doFpop'' List{items = as, len = al} List{items = bs', len = bl + 1} (index + 1)  
    |   otherwise = List{items = bs, len = bl} 

--Fetches an item from a list of a specific index.
doIndex :: EDState -> IO EDState
doIndex state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (index) error. Two operands required for index!"
        [x] -> error "Operator (index) error. Two operands required for index!"
        vals ->  
            let (state', list, index) = fsPop2 state
                ret = (\x -> return x)
            in ret $! (doIndex' state' list index) 

--Retrieves item at index in list or string.
doIndex' :: EDState -> Value -> Value -> EDState
doIndex' state (List {items = _, len = 0}) _ = error "Can't index into empty list."
doIndex' state (String {chrs = "", len = 0}) _ = error "Can't index into empty string."
doIndex' state (List {items = is, len = l}) (Integer index) = 
    let state' = fsPush (List {items = is, len = l}) state
        indexed = case (M.lookup index is) of 
            Just i -> i 
            Nothing -> error ("Operator (index) error. Index " ++ (show index) ++ " out of valid range for list.")
        push = (\x -> fsPush x state')
    in push $! indexed
--String case.
doIndex' state (String {chrs = cs, len = l}) (Integer index) = 
    let state' = fsPush (String {chrs = cs, len = l}) state
        accessIndex = 
            if (index > -1 && index < l) 
                then index 
                else error ("Operator (index) error. Index " ++ (show index) ++ " out of valid range for string.")
        push = (\x -> fsPush (Char $ cs !! x) state')
    in push $! accessIndex
doIndex' _ _ _ = error "Operator (index) error. Index operator needs a list/string and an index value. \n Index must use type Integer to perform an index"

--Takes the length of a list or string at the top 
-- of the stack and pushes resulting length to top of stack.
doLength :: EDState -> IO EDState
doLength state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (length) error. Operand needed for length!"
        vals -> 
            let top = (fsTop state)
                ret = (\x -> return x)
            in ret $! (doLength' state top)

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
        vals -> 
            let top = (fsTop state)
                ret = (\x -> return x)
            in ret $! (doIsEmpty' state top) 

--Performs actual length function.
doIsEmpty' :: EDState -> Value -> EDState
doIsEmpty' state (List {items = is, len = l}) = fsPush (Boolean $ l == 0) state
doIsEmpty' state (String {chrs = cs, len = l}) = fsPush (Boolean $ null cs) state
doIsEmpty' state Object{fields = fs} = fsPush (Boolean $ M.null fs) state 
doIsEmpty' state _ = error "Operator (isEmpty) error. List or string type is needed to test for emptyness."

--Sets the string or list at the top of the stack to empty.
doClear :: EDState -> IO EDState
doClear state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (clear) error. One operand needed!"
        vals ->  
            let (state', list) = fsPop state
                ret = (\x -> return x)
            in ret $! (doClear' state' list) 

--Performs clear operation.
doClear' :: EDState -> Value -> EDState
doClear' state (List {items = _, len = _}) = fsPush (List {items = M.empty, len = 0}) state
doClear' state (String {chrs = _, len = _}) = fsPush (String {chrs = "", len = 0}) state
doClear' state (Object{fields = _}) = fsPush (Object{fields = M.empty}) state
doClear' state _ = error "Operator (clear) error. List or string is needed for clear to occur."

--Used to turn a value of one type into another.
doCast :: EDState -> IO EDState
doCast state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (cast) error. Two operands required for cast!"
        [x] -> error "Operator (cast) error. Two operands required for cast!"
        vals ->  
            let (state', val, castType) = fsPop2 state
                ret = (\x -> return x)
            in ret $! (doCast' state' val castType) 

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
doCast' state (BigInteger n) (String{chrs = "Char", len = _}) =
    let nInt = fromIntegral n :: Int
    in if validIntToChar nInt 
        then fsPush (Char $ chr nInt) state
        else error ("\nOperator (cast) error. " 
            ++ "Failed to convert type BigInteger to Char." ++ 
            "\nTry making sure the Integer is in the UTF-8 numerical range.\n"
            ++ "Given value: " ++ (show n) ++ " valid numbers are " ++ (show $ ord minBound) ++ " to " ++ (show $ ord maxBound) ++ ".") 

doCast' state (Integer n) (String {chrs = "String", len = _}) = fsPush (String {chrs = show n, len = length $ show n}) state
doCast' state (Integer n) (String {chrs = "Integer", len = _}) = fsPush (Integer n) state --Do nothing case
doCast' state (Integer n) (String {chrs = "BigInteger", len = _}) = fsPush (BigInteger (fromIntegral n :: Integer)) state 
doCast' state (Integer n) (String {chrs = "Float", len = _}) = fsPush (Float (fromIntegral n :: Float)) state
doCast' state (Integer n) (String {chrs = "Double", len = _}) = fsPush (Double (fromIntegral n :: Double)) state
doCast' state (Integer n) (String{chrs = "Char", len = _}) = 
    if validIntToChar n 
        then fsPush (Char $ chr n) state
        else error ("\nOperator (cast) error. " 
            ++ "Failed to convert type Integer to Char." ++ 
            "\nTry making sure the Integer is in the UTF-8 numerical range.\n"
            ++ "Given value: " ++ (show n) ++ " valid numbers are " ++ (show $ ord minBound) ++ " to " ++ (show $ ord maxBound) ++ ".") 

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

doCast' state (Char c) (String {chrs = "Integer", len = _}) = fsPush (Integer $ ord c) state
doCast' state (Char c) (String {chrs = "BigInteger", len = _}) = fsPush (BigInteger (fromIntegral (ord c) :: Integer)) state

doCast' state (String {chrs = cs, len = l}) (String {chrs = "Integer", len = _}) = 
    let mbyInt = readMaybe cs :: Maybe Int
        parsed = case mbyInt of 
                    Just val -> val 
                    Nothing -> error ("Operator (cast) error.\nFailed to convert String \"" ++ cs ++ "\" to type Integer.")
        push = (\x -> fsPush (Integer x) state) 
    in push $! parsed

doCast' state (String {chrs = cs, len = l}) (String {chrs = "BigInteger", len = _}) = 
    let mbyBigInt = readMaybe cs :: Maybe Integer
        parsed = case mbyBigInt of 
                    Just val -> val 
                    Nothing -> error ("Operator (cast) error.\nFailed to convert String \"" ++ cs ++ "\" to type BigInteger.")
        push = (\x -> fsPush (BigInteger x) state)
    in push $! parsed

doCast' state (String {chrs = cs, len = l}) (String {chrs = "Float", len = _}) = 
    let mbyFlt = readMaybe cs :: Maybe Float
        parsed = case mbyFlt of 
                    Just val -> val 
                    Nothing -> error ("Operator (cast) error.\nFailed to convert String \"" ++ cs ++ "\" to type Float.")
        push = (\x -> fsPush (Float x) state)
    in push $! parsed

doCast' state (String {chrs = cs, len = l}) (String {chrs = "Double", len = _}) = 
    let mbyDbl = readMaybe cs :: Maybe Double
        parsed = case mbyDbl of 
                    Just val -> val 
                    Nothing -> error ("Operator (cast) error.\nFailed to convert String \"" ++ cs ++ "\" to type Double.")
        push = (\x -> fsPush (Double parsed) state)
    in push $! parsed

doCast' state (List{items = is, len = l}) (String{chrs = "String", len = _}) = 
    let listStr = ( "[" ++ (printList List {items = is, len = l} "" 0 False) ++ "]" )
        listStrLen = length listStr 
    in fsPush (String{chrs = listStr, len = listStrLen}) state

doCast' state (Object{fields = fs}) (String{chrs = "String", len = _}) = 
    let objStr = ("{" ++ (printObj (M.toList fs) "") ++ "}")
        objStrLen = length objStr
    in fsPush (String{chrs = objStr, len = objStrLen}) state

doCast' state val _ = error "Operator (cast) error.\nSecond argument of cast needs to be string or invalid cast configuration was given."

--Determines if the number is 
-- in the valid UTF-8 character number range for casting.
validIntToChar :: Int -> Bool
validIntToChar num = (num >= (ord minBound)) && (num <= (ord maxBound))

--Prints top element of stack. This element must be a string or it freaks out.
doPrintLine :: EDState -> IO EDState
doPrintLine state = do 
    let stck = (stack state)
    if (null stck) 
        then error "Operator (printLine) error. Can't print from empty stack!"
        else case (head stck) of 
            String {chrs = cs, len = l} -> putStrLn cs 
            _ -> error "Operator (printLine) error. Top of stack must be a String to be printed!"
    return state

--Writes a string to stdout without adding a newline automatically to the end.
doPrint :: EDState -> IO EDState
doPrint state = do 
    let stck = stack state
    if (null stck)
        then error "Operator (print) error. One operand needed for print!"
        else case (head stck) of 
            String{chrs = cs, len = l} -> putStr cs 
            _ -> error "Operator (print) error. Top of stack needs to be a Sring to be printed!"
    return state

--Reads a line from stdin, and pushes it onto stack.
doReadLine :: EDState -> IO EDState
doReadLine state = do 
    let stck = (stack state)
    input <- getLine
    return (fsPush (String{chrs = input, len = length input}) state)

--Reads a multi-line string from stdin.
doRead :: EDState -> IO EDState
doRead state = do 
    captured <- doRead' ""
    return (fsPush (String{chrs = captured, len = length captured}) state)

--Prints a desired error to stdout.
doPrintError :: EDState -> IO EDState
doPrintError state = do 
    let stck = stack state
    case stck of 
        ((String{chrs = err, len = _}):xs) -> throwError err state
        _ -> throwError "\nOperator (printError) error.\nString needed on top of stack for error to print." state

--Reads a multi-line string from stdin until 
-- an empty string is read or EOF is hit.
doRead' :: String -> IO String 
doRead' acc = do 
    isEnd <- isEOF
    if (isEnd) 
        then return acc
        else do 
            input <- getLine 
            if (null input)
                then return acc 
                else doRead' (acc ++ input ++ ['\n'])

--Prints a char to stdout given at top of stack.
doPrintChar :: EDState -> IO EDState
doPrintChar state = do 
    let stck = stack state
    if (null stck)
        then error "Operator (printChar) error. Can't print empty stack!"
        else case (head stck) of 
            Char c -> putChar c 
            _ -> error "Operator (printChar) error. Top of stack must be type Char when printed!"
    return state

--Reads a Char from stdin and pushes it to the stack.
doReadChar :: EDState -> IO EDState 
doReadChar state = do 
    let stck = stack state
    inChar <- getChar 
    return (fsPush (Char inChar) state)

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

--Determines if a list, string, 
-- or object contains a value, char, or field, respectively.
doContains :: EDState -> IO EDState
doContains state = do 
    let stck = (stack state)
    case stck of 
        [] -> error "Operator (contains) error. Two operands on stack needed!"
        [x] -> error "Operator (contains) error. Two operands on stack needed!"
        vals -> 
            let (top, secondToTop) = ((head stck), (head $ tail stck))
                contains = case (top, secondToTop) of 
                                (v, List {items = is, len = _}) -> v `elem` is
                                (Char c, String {chrs = cs, len = _}) -> c `elem` cs
                                (String{chrs = name, len = _}, Object{fields = fs}) -> 
                                    case (M.lookup name fs) of 
                                        Just _ -> True 
                                        Nothing -> False
                                (_, _) -> error "Operator (contains) error.\nFirst pushed element must be List, String, or Object,\n and second item needs to be value, Char, or String, respectively."
                ret = (\x -> return (fsPush (Boolean x) state) )
            in ret $! contains

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
                (List {items = is, len = l}, v, Integer i) -> return (fsPush ( List { items = M.insert i v is, len = l } ) state')
                (_, _, _) -> error "Operator (changeItemAt) error. List, value, and Integer type in that order needed on stack."

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

--Adds a field to a given object.
doAddField :: EDState -> IO EDState
doAddField state = do 
    let stck = stack state
    case stck of 
        [] -> error "Operator (addField) error. Three operands needed!"
        [x] -> error "Operator (addField) error. Three operands needed!"
        [x, y] -> error "Operator (addField) error. Three operands needed!"
        vals ->  
            let (state', obj, fieldName, fieldVal) = fsPop3 state
                obj' = case (obj, fieldName, fieldVal) of 
                        (Object {fields = fs}, String {chrs = name, len = l}, v) ->
                            (doAddField' Object{fields = fs} name v)  
                        
                        (_, _, _) -> error "Operator (addField) error.\nOperands need to be type Object String Value!"
                ret = (\x -> return (fsPush x state'))
            in ret $! obj'

doAddField' :: Value -> String -> Value -> Value
doAddField' Object{fields = fs} key val = 
    let fs' = case (M.lookup key fs) of 
            Just i -> error ("Operator (addField) error.\nField " ++ key ++ " already exists in given object!")
            Nothing -> M.insert key val fs
        newObj = (\x -> Object{fields = x})
    in newObj $! fs'

--Removes a field from a given object. Does nothing if the field doesn't exist.
doRemoveField :: EDState -> IO EDState
doRemoveField state = do 
    let stck = stack state
    case stck of 
        [] -> error "Operator (removeField) error. Two operands needed!"
        [x] -> error "Operator (removeField) error. Two operands needed!"
        vals -> do 
            let (state', obj, removalKey) = fsPop2 state
            case (obj, removalKey) of 
                (Object{fields = fs}, String{chrs = name, len = l}) -> do 
                    let fs' = case (M.lookup name fs) of 
                            Just i -> (M.delete name fs)
                            Nothing -> error ("Operator (removeField) error.\nField " ++ name ++ " doesn't exist in given object!")
                        ret = (\x -> return (fsPush Object{fields = x} state')) 
                    ret $! fs'
                (_, _) -> error "Operator (removeField) error.\nOperands need to be type Object and String."

--Grabs the value of a field in an object or throws an error if it doesn't exist.
doGetField :: EDState -> IO EDState
doGetField state = do 
    let stck = stack state
    case stck of 
        [] -> error "Operator (getField) error. Two operands needed!"
        [x] -> error "Operator (getField) error. Two operands needed!"
        vals -> do 
            let (state', obj, findKey) = fsPop2 state 
            case (obj, findKey) of 
                (Object{fields = fs}, String{chrs = name, len = l}) -> 
                    let lkup = case (M.lookup name fs) of 
                            Just i -> i
                            Nothing -> error ("Operator (getField) error.\nField " ++ name ++ " doesn't exist in given object!")
                        state'' = fsPush Object{fields = fs} state'
                        ret = (\x -> return (fsPush x state''))
                    in ret $! lkup
                (_, _) -> error "Operator (getField) error.\nOperands need to be type Object and String."

--Mutates the value of a field in the object assuming the field exists 
-- and the types match for the old and new values.
doMutateField :: EDState -> IO EDState
doMutateField state = do 
    let stck = stack state 
    case stck of 
        [] -> error "Operator (mutateField) error. Three operands needed!"
        [x] -> error "Operator (mutateField) error. Three operands needed!"
        [x, y] -> error "Operator (mutateField) error. Three operands needed!"
        vals -> do 
            let (state', obj, mutKey, newVal) = fsPop3 state 
            case (obj, mutKey) of 
                (Object{fields = fs}, String{chrs = name, len = l}) -> 
                    let fs' = case (M.lookup name fs) of 
                            Just i -> if (compareTypesForMut i newVal) 
                                then (M.insert name newVal fs) 
                                else error ("Operator (mutateField) error.\nNew value for field " ++ name ++ " must match type of current field value!")
                            Nothing -> error ("Operator (mutateField) error.\nField " ++ name ++ " doesn't exist in given object!")
                        ret = (\x -> return (fsPush Object{fields = x} state'))
                    in ret $! fs'
                (_, _) -> error "Operator (mutateField) error.\nOperands need to be type Object and String."

--Reads in the contents of a file to a string.
doReadFile :: EDState -> IO EDState 
doReadFile state = do 
    let stck = stack state
    case stck of 
        [] -> error "Operator (readFile) error. One operand needed!"
        vals -> do 
            let (state', fileName) = fsPop state 
            case (fileName) of 
                    (String{chrs = cs, len = l}) -> do 
                        withFile cs ReadMode $ \file -> do 
                            fileStr <- hGetContents file
                            let state'' = fsPush (String{chrs = fileStr, len = length fileStr}) state' 
                            fileStr `deepseq` return (state'')
                    _ -> error "Operator (readFile) error.\nOperand needs to be of type String."

--Writes the desired string to a given file name.
doWriteFile :: EDState -> IO EDState
doWriteFile state = do 
    let stck = stack state
    case stck of 
        [] -> error "Operator (writeFile) error. Two operands needed!"
        [x] -> error "Operator (writeFile) error. Two operands needed!"
        vals -> do 
            let (state', fileName, writeContents) = fsPop2 state
            case (fileName, writeContents) of 
                (String{chrs = name, len = _}, String{chrs = contents, len = l}) -> do 
                    withFile name WriteMode $ \file -> do 
                        hPutStr file contents  
                        return (state')
                (_, _) -> error "Operator (writeFile) error.\nOperands need to be of type String and String."

--Determines the type of an item on the stack.
doQueryType :: EDState -> IO EDState
doQueryType state =  
    let stck = stack state
    in case stck of 
        [] -> error "Operator (queryType) error. One operand needed!"
        vals -> 
            let (state', val) = fsPop state
            in return (fsPush (doQueryType' val) state)

--Uses pattern matching to find type of given value.
doQueryType' :: Value -> Value
doQueryType' (BigInteger _) = String{chrs = "BigInteger", len = length "BigInteger"}
doQueryType' (Integer _) = String{chrs = "Integer", len = length "Integer"}
doQueryType' (Float _) = String{chrs = "Float", len = length "Float"}
doQueryType' (Double _) = String{chrs = "Double", len = length "Double"}
doQueryType' String{chrs = _, len = _} = String{chrs = "String", len = length "String"}
doQueryType' (Char _) = String{chrs = "Char", len = length "Char"}
doQueryType' (Boolean _) = String{chrs = "Boolean", len = length "Boolean"}
doQueryType' (List {items = _, len = _}) = String{chrs = "List", len = length "List"}
doQueryType' (Object {fields = _}) = String{chrs = "Object", len = length "Object"}

--Prints stack to stdout when called.
doDebugPrintStack :: EDState -> IO EDState
doDebugPrintStack state = do 
    putStrLn "----------------------------------------------\nDEBUG START"
    putStrLn "STACK START"
    printStack (reverse $ stack state)
    putStrLn "STACK END"
    putStrLn ("STACK LENGTH: " ++ (show $ length $ stack state))
    putStrLn "DEBUG END\n----------------------------------------------"
    return state

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
doOp "dropStack" = doDropStack
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
doOp "queryType" = doQueryType
--IO stuff
doOp "printLine" = doPrintLine
doOp "readLine" = doReadLine
doOp "printChar" = doPrintChar
doOp "readChar" = doReadChar
doOp "print" = doPrint
doOp "read" = doRead

--Error display
doOp "printError" = doPrintError

--Debug Operator
doOp "debugPrintStack" = doDebugPrintStack

--Object Operators
doOp "addField" = doAddField 
doOp "removeField" = doRemoveField 
doOp "getField" = doGetField 
doOp "mutateField" = doMutateField 

--File IO Operators
doOp "readFile" = doReadFile 
doOp "writeFile" = doWriteFile

-- Error thrown if reached here.
doOp op = error $ "unrecognized word: " ++ op 

astNodeToString :: AstNode -> String
astNodeToString (Terminal (Word w)) = w

--Creates a local variable in the current scope. 
-- Throws error if variable already exists.
makeLoc :: EDState -> String -> IO EDState
makeLoc state name = 
    let look = M.lookup name (head $ frames state)
    in case look of 
        Just _ -> error ("Loc Mak Error:\nlocal Variable " ++ name ++ " already exists in current scope.") 
        Nothing -> do 
            let top = fsTop state
            let frame' = M.insert name top (head $ frames state)
            return (EDState{stack = (stack state), fns = (fns state), vars = (vars state), frames = (frame' : (tail $ frames state))})

--Performs a lookup through all stack frames to find desired variable name.
--If found, returns it, else returns Nothing.
getLoc :: [M.Map String Value] -> String -> Maybe Value
getLoc [] name = Nothing
getLoc (f:fs) name = 
    let look = M.lookup name f 
    in case look of 
        Just v -> Just v
        Nothing -> getLoc fs name  

makeVar :: EDState -> String -> IO EDState
makeVar state varName = 
    let lkup = M.lookup varName (vars state)
    --Throw error if variable exists. Otherwise, make variable by inserting it into hash table.
    in case lkup of
        Just _ -> error ("Variable Mak Error: Variable " ++ varName ++ " already exists.")
        Nothing -> do 
            let top = fsTop state
            let vars' = M.insert varName top (vars state)
            return (EDState{stack = (stack state), fns = (fns state), vars = vars', frames = (frames state)})

--Recursively builds a new list of stack frames with the mutated variable in it.
mutateLoc' :: [M.Map String Value] -> [M.Map String Value] -> Value -> String -> [M.Map String Value]
mutateLoc' [] acc mutVal name = error ("SHOULD NEVER GET HERE!!!!!!")
mutateLoc' (f:fs) acc mutVal name = 
    let look = M.lookup name f 
    in case look of 
            Just _ -> 
                let f' = M.insert name mutVal f
                in acc ++ (f':fs) 
            Nothing -> mutateLoc' fs (acc ++ [f]) mutVal name

--Mutates a given local variable.
mutateLoc :: EDState -> String -> IO EDState
mutateLoc state name =
    let locVarToChange = case (getLoc (frames state) name) of 
            Just v -> v 
            Nothing -> error ("Loc Mut Error:\nLocal Variable " ++ name ++ " not defined for mutation.")
        newVal = fsTop state

    in if (compareTypesForMut newVal locVarToChange) 
            then
                return (EDState{stack = (stack state), fns = (fns state), vars = (vars state), frames = (mutateLoc' (frames state) [] newVal name)})
            else error ("Loc Mut Error:\nCan't mutate local variable " ++ name ++ " to different type.")


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
compareTypesForMut Object{fields = _} Object{fields = _} = True 
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
            in return ( EDState{stack = (stack state), fns = (fns state), vars = vars', frames = (frames state)} )
            else error ("Variable Mut Error: Can't mutate variable " ++ varName ++ " to different type.")
        Nothing -> error ("Variable Mut Error: Variable " ++ varName ++ " doesn't exist or was deleted")

--Defines a function as desired.
funcDef :: EDState -> String -> AstNode -> EDState
funcDef state funcName funcBod = 
    let look = M.lookup funcName (fns state)
    in case look of 
        Just bod -> error ("Function Def Error: Function of same name \"" ++ funcName ++ "\" already exists") 
        Nothing -> let fns' = M.insert funcName funcBod (fns state)
                   in EDState{stack = (stack state), fns = fns', vars = (vars state), frames = (frames state)}

--Calls a given function and runs it.
funcCall :: EDState -> String -> (EDState, AstNode)
funcCall state funcName = 
    let look = M.lookup funcName (fns state)
    in case look of 
        Just body -> (state, body)
        Nothing -> error ("Function Call Error: Function \"" ++ funcName ++ "\" isn't defined.")

--Used to add a stack frame when the scope increases. 
addFrame :: EDState -> EDState
addFrame state = EDState {stack = (stack state), fns = (fns state), vars = (vars state), frames = ((M.empty):(frames state))}

--Removes stack frame if not at global scope.
removeFrame :: EDState -> EDState
removeFrame EDState{stack = s, fns = f, vars = v, frames = [x]} = 
    EDState{stack = s, fns = f, vars = v, frames = [x]}
removeFrame EDState{stack = s, fns = f, vars = v, frames = (x:xs)} =
    EDState{stack = s, fns = f, vars = v, frames = (xs)}

--Runs through the code and executes all nodes of the AST.
doNode :: AstNode -> EDState -> IO EDState

--Attempt on Error doNode case where it tries to run code in attempt branch 
-- and instead runs the code in on Error if there's a problem.
doNode AttErr{attempt = att, onError = err} state = catch (doNode att (addFrame state)) handler 
    where 
        handler :: GeneralException -> IO EDState
        handler (GeneralException msg) = 
            let state' = fsPush (String {chrs = msg, len = length msg}) (addFrame state)
            in doNode err (addFrame state')
    

-- Runs true branch if top of stack is true 
--and false branch if top of stack is false.
doNode If { ifTrue = trueBranch, ifFalse = falseBranch } state = do
    let isStackEmpty = (null (stack state))
    let top = if isStackEmpty 
        then error "If statement error:\nNo boolean value for if to check because stack is empty." 
        else fsTop state

    --Runs true branch if top is true, false if false, and errors out otherwise.
    case top of 
        (Boolean True) -> doNode trueBranch (addFrame state)
        (Boolean False) -> doNode falseBranch (addFrame state)
        _ -> error "If statement error:\nIf statement requires top of stack to be type Boolean to perform valid branching!"

--Patterm matches function definition.
doNode (Expression((Function {funcCmd = cmd, funcName = name, funcBod = body}):rest)) state =
    case (astNodeToString cmd) of 
        "def" -> 
            let dnode = (\x -> doNode (Expression(rest)) x) 
            in dnode $! (funcDef state (astNodeToString name) body)

        "call" -> do 
                    let (state', funcBod) = funcCall state (astNodeToString name)
                    state'' <- (doNode funcBod (addFrame state') )
                    doNode (Expression(rest)) state''
        other -> error ("Function Error: Invalid function command given.\nGiven: " ++ other ++ "\nValid: def, call")

--Runs all the different cases of variable actions.
doNode (Expression((Variable{varName = name, varCmd = cmd}):rest)) state =
    case (astNodeToString cmd) of
        "mak" -> do 
            let stackIsEmpty = null (stack state)
            if stackIsEmpty
                then error ("Variable Mak Error: Can't create variable when stack is empty.\nAttempted variable name: " ++ (astNodeToString name))
                else do
                    state' <- (makeVar state (astNodeToString name))
                    doNode (Expression rest) state'
                           
        "get" -> let lkup = M.lookup (astNodeToString name) (vars state) 
                 in case lkup of
                    Just value -> let stack' = fsPush value state
                              in doNode (Expression rest) stack' 
                    Nothing -> error ("Variable Get Error: Variable " ++ (astNodeToString name) ++ " doesn't exist or was deleted.")

        "del" -> let lkup = M.lookup (astNodeToString name) (vars state) 
                 in case lkup of
                    Just value -> let vars' = M.delete (astNodeToString name) (vars state)
                           in doNode (Expression rest) (EDState{stack = (stack state), fns = (fns state), vars = vars', frames = (frames state)})
                    Nothing -> error ("Variable Del Error: Variable " ++ (astNodeToString name) ++ " doesn't exist.") 

        "mut" -> do 
            let stackIsEmpty = null (stack state)
            if stackIsEmpty
                then error ("Variable Mut Error: Can't mutate variable when stack is empty.\nAttempted variable name: " ++ (astNodeToString name))
                else do 
                    state' <- (mutateVar state (astNodeToString name))
                    doNode (Expression rest) state'

        other -> error ("Variable Command Error: Invalid variable command given.\nGiven: " ++ other ++ "\nValid: mak, get, mut, del")

--Runs all the different cases of local variable actions.
doNode (Expression((LocVar{name = name, cmd = cmd}):rest)) state =
    case (astNodeToString cmd) of
        "mak" ->
            let stackIsEmpty = null (stack state)
            in if stackIsEmpty
                then error ("Loc Mak Error: Can't create variable when stack is empty.\nAttempted local variable name: " ++ (astNodeToString name))
                else do
                    state' <- (makeLoc state (astNodeToString name))
                    doNode (Expression rest) state'
                           
        "get" -> let findRes = getLoc (frames state) (astNodeToString name) 
                 in case findRes of
                    Just value -> let state' = fsPush value state
                              in doNode (Expression rest) state' 
                    Nothing -> error ("Loc Get Error:\nLocal Variable " ++ (astNodeToString name) ++ " not defined.")

        "mut" -> 
            let stackIsEmpty = null (stack state)
            in if stackIsEmpty
                then error ("Loc Mut Error: Can't mutate local variable when stack is empty.\nAttempted local variable name: " ++ (astNodeToString name))
                else do 
                    state' <- (mutateLoc state (astNodeToString name))
                    doNode (Expression rest) state'

        other -> error ("Local Variable Command Error: Invalid local variable command given.\nGiven: " ++ other ++ "\nValid: mak, get, mut")

--Runs while loop.                                                                                                                      
doNode ( While loopBody ) state = do
    let stackIsEmpty = null (stack state)
    let top = if stackIsEmpty 
        then error "While Loop error:\nNo boolean value for while loop to check because stack is empty." 
        else fsTop state

    --Creates new state if loop body runs.
    -- Otherwise newState is same as state.
    -- Errors out if top of stack isn't a boolean type.
    newState <- case top of 
        (Boolean True) -> doNode (loopBody) (addFrame state)
        (Boolean False) -> return state
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
doNode ( Terminal ( Val v ) ) state = return $ fsPush v state

-- ...if it's a word, execute the operation
doNode ( Terminal ( Word o ) ) state = doOp o state

-- "doing" an empty expression does nothing
doNode ( Expression [] ) state = return $ removeFrame state

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

    --Parse basic variable case.
    | token == Word "var" = 
        let (varAct, variableName, remTokens) = parseVarAction tokens
            newParsed = alreadyParsed ++ [Variable{varName = variableName, varCmd = varAct}]
        in parseExpression' newParsed remTokens terminators

    --Parse local variable.
    | token == Word "loc" =
        let (varAct, variableName, remTokens) = parseVarAction tokens
            newParsed = alreadyParsed ++ [LocVar{name = variableName, cmd = varAct}]
        in parseExpression' newParsed remTokens terminators

    --Parse attErr block.
    | token == Word "attempt" =
        let (attemptBranch, errorBranch, remTokens) = parseAttErr tokens
        in parseExpression' (alreadyParsed ++ [AttErr{attempt = attemptBranch, onError = errorBranch}]) remTokens terminators
        
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

--Parses an attErr code block into its appropriate expression.
parseAttErr :: [Token] -> (AstNode, AstNode, [Token])
parseAttErr tokens = 
    let (attBranch, remainingTokens, terminator ) = parseExpression' [] tokens [ "onError", ";" ]
        (errBranch, remTokens) = if terminator == (Just $ Word "onError") 
            then parseErrorBranch remainingTokens 
            else error "attempt onError Error:\n Branch onError branch missing.\nUSAGE: attempt CODE_TO_ATTEMPT onError CODE_TO_HANDLE_ERROR ;"
    in (Expression(attBranch), errBranch, remTokens)

--Parses the onError branch of attErr.
parseErrorBranch :: [Token] -> (AstNode, [Token])
parseErrorBranch tokens = 
    let (errorHandleCode, remTokens, terminator) = parseExpression' [] tokens [";"]
    in (Expression(errorHandleCode), remTokens)

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
    
--Makes new interpretor state with default values.
fsNew :: IO EDState
fsNew = return EDState { stack = [], fns = M.empty, vars = M.empty, frames = [M.empty]}

-- push a new value onto the stack
fsPush :: Value -> EDState -> EDState
fsPush val state = EDState { stack = (val : (stack state)), fns = (fns state), vars = (vars state), frames = (frames state)}

--Removes value from top of stack, returning it.
fsPop :: EDState -> ( EDState, Value )
fsPop state = 
    let top = head (stack state) 
        newStack = tail (stack state)  
    in  ( EDState { stack = newStack, fns = (fns state), vars = (vars state), frames = (frames state) }, top )

--Removes the top two elements from the stack, returning them.
fsPop2 :: EDState -> ( EDState, Value, Value )
fsPop2 state = 
    let (state', top) = fsPop state
        (state'', secondToTop) = fsPop state'
    in  (EDState {stack = (stack state''), fns = (fns state), vars = (vars state), frames = (frames state)}, secondToTop, top)

--Removes top three elements from stack.
fsPop3 :: EDState -> ( EDState, Value, Value, Value )
fsPop3 state = 
    let (state', top) = fsPop state
        (state'', secondToTop) = fsPop state'
        (state''', thirdToTop) = fsPop state''
    in (EDState {stack = (stack state'''), fns = (fns state), vars = (vars state), frames = (frames state)}, thirdToTop, secondToTop, top)

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
    | t == "[]" = Val $ List {items = M.empty, len = 0}  --Empty list case.
    | t == "{}" = Val $ Object {fields = M.empty} --Empty object case.
    | (head t) == '"' && (last t) == '"' =
        let str = read t :: String
        in Val $ String { chrs = str, len = length str }  --String case
    | isValidChar t = Val $ Char (read t :: Char) --Char case.
    | (last t == 'b') && ((isNum (if head t == '-' then tail $ init t else init t)) == 0) = Val $ BigInteger (read (init t) :: Integer) --BigInteger case
    | (last t == 'd') && ((isNum (if head t == '-' then tail $ init t else init t)) == 1) = Val $ Double (read (init t) :: Double) -- Double case
    | (isNum (if head t == '-' then tail t else t)) == 0 = Val $ Integer (read t :: Int) --Int Case
    | (isNum (if head t == '-' then tail t else t)) == 1 = Val $ Float (read t :: Float) --Float case
    | otherwise = Word t                             

--Determines if a string can be casted to a char.
isValidChar :: String -> Bool
isValidChar str = 
    let parseRes = readMaybe str :: Maybe Char
        isValid = case parseRes of 
            Just _ -> True
            Nothing -> False
    in isValid 

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
    putStrLn ("[" ++ (printList List {items = is, len = l} "" 0 True) ++ (if (l > 16) then ", ...]" else "]")) >> printStack xs
printStack ((String {chrs = cs, len = l}):xs) =
    let pr = if l < 256 then cs else (init  $ take 255 cs) ++ "..."
    in putStrLn (show (String {chrs = pr, len = l})) >> printStack xs
printStack ((Object{fields = fs}):xs) = putStrLn ("{" ++ (printObj (M.toList fs) "") ++ "}") >> printStack xs
printStack (x:xs) = print x >> printStack xs

--Recursively prints a list's contents.
printList :: Value -> String -> Int -> Bool -> String
printList List {items = is, len = l} acc index isLimited 
    | (index < l) && (index < 16 || isLimited == False) = 
        let curr = case M.lookup index is of
                Just i -> i 
                Nothing -> error "SHOULD NEVER GET HERE!!!"
                                          
            acc' = case curr of 
                List {items = ls, len = listLength} -> acc ++ (if (accSmall acc) 
                    then ", [" 
                    else "[") ++ (printList (List{items = ls, len = listLength}) "" 0 isLimited) ++ (if (isLimited && listLength > 16) then ", ...]" else "]")
                Object {fields = fs} -> acc ++ (if (accSmall acc) then ", {" else "{") ++ (printObj (M.toList fs) "") ++ "}"
                i -> acc ++ (if (index > 0) then ", " else "") ++ (show i)
        in printList (List{items = is, len = l}) acc' (index + 1) isLimited 
    | otherwise = acc 

--Prints a given object's fields.
printObj :: [(String, Value)] -> String -> String
printObj [] acc = acc 
printObj ((name, val):xs) acc = 
    let insStr = case val of 
            Object{fields = fs} -> "{" ++ (printObj (M.toList fs) "") ++ "}"
            List{items = is, len = l} -> "[" ++ (printList (List{items = is, len = l}) "" 0 True) ++ (if (l > 16) then ", ...]" else "]")
            String{chrs = cs, len = l} -> 
                let cs' = if l < 256 then cs else (init $ take 255 cs) ++ "..."
                in show $ String{chrs = cs', len = l}
            i -> show i

    in printObj xs (acc ++ (if accSmall acc then ", " else "") ++ name ++ " : " ++ insStr)

--Uses pattern matching to determine if a special comma 
-- and space needs to be added or not.
accSmall :: String -> Bool
accSmall "" = False
accSmall _ = True

main :: IO ()
main = do
    args <- getArgs

    let fileName = if (not $ null args) 
        then (args !! 0) 
        else error "Please provide an EcksDee code file to parse!"

    withFile fileName ReadMode $ \file -> do   
        code <- hGetContents file

        -- convert it into a list of tokens
        let tokens = code `deepseq` removeComments False [] ( tokenize code )

        --Parse and run AST, printing result.
        let ast = parseExpression tokens
        stateInit <- fsNew
        finalState <- (doNode ast stateInit)
        
        --putStrLn $ show $ length $ frames finalState

        printStack $ reverse $ (stack finalState) 