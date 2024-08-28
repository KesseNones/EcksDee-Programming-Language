--Jesse A. Jones
--Version: 2024-08-28.228
--Toy Programming Language Named EcksDee

{-
    ISSUES:
        -Extra error checking for casting is a good idea.
        -Maybe have errors show line number 
            of code file where error happened, somehow. 
            It would make user debugging much less ass.
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
import qualified Data.HashMap.Strict as HM
import Control.DeepSeq
import Control.Exception
import Data.Typeable
import System.IO.Error (tryIOError)
import Data.Bits

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
    |   Box Int
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

    |   TempStackChange AstNode

    |   BoxOp AstNode

    deriving ( Show )

data Heap = Heap {
    freeList :: M.Map Int (),
    h :: M.Map Int Value,
    heapSize :: Int
}

-- This is the state of the interpreter. 
-- Currently it stores the stack, which is where all of the data lives. 
data EDState = EDState { 
    stack :: [Value], 
    fns :: M.Map String AstNode,
    vars :: M.Map String Value,
    frames :: [M.Map String Value],
    heap :: Heap,
    ops :: HM.HashMap String (EDState -> Either EDState String),
    ioOps :: HM.HashMap String (EDState -> IO EDState) 
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
divideVals :: Value -> Value -> Either Value String
divideVals (BigInteger a) (BigInteger b) = if (a /= 0) then Left $ BigInteger (b `div` a) else Right "Operator (/) error. Can't divide by zero for type BigInteger!"
divideVals (Integer a) (Integer b) = if (a /= 0) then Left $ Integer (b `div` a) else Right "Operator (/) error. Can't divide by zero for type Integer!"
divideVals (Double a) (Double b) = if (a /= 0.0) then Left $ Double (b / a) else Right "Operator (/) error. Can't divide by zero for type Double!"
divideVals (Float a) (Float b) = if (a /= 0.0) then Left $ Float (b / a) else Right "Operator (/) error. Can't divide by zero for type Float!"
divideVals a b =
    let (bType, aType) = findTypeStrsForError b a  
    in Right ("Operator (/) error. Can't divide types that are not both types of BigIntegers, Integers, Floats, or Doubles! " 
        ++ "Attempted types were: " 
        ++ bType ++ " and " ++ aType)

--Performs modulo operation on two values.
modVals :: Value -> Value -> Either Value String
modVals (BigInteger a) (BigInteger b) = Left $ BigInteger (b `mod` a)
modVals (Integer a) (Integer b) = Left $ Integer (b `mod` a)
modVals a b =
    let (bType, aType) = findTypeStrsForError b a  
    in Right ("Operator (%) error. Can't modulo types that are not both types of BigIntegers, or Integers! " 
        ++ "Attempted types were: " 
        ++ bType ++ " and " ++ aType)

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
doConcat' :: Value -> Value -> Either Value String
doConcat' (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Left $ String {chrs = acs ++ bcs, len = al + bl}
doConcat' List {items = as, len = al} List {items = bs, len = bl} = 
    let List{items = cs, len = cl} = doConcat'' List{items = as, len = al} List{items = M.empty, len = 0} 0 0
        List{items = ds, len = dl} = doConcat'' List{items = bs, len = bl} List{items = cs, len = cl} 0 al
    in Left $ List{items = ds, len = dl}
doConcat' a b =
    let (aType, bType) = findTypeStrsForError a b  
    in Right ("Operator (++) error. Can't concatenate types that are not both types of List or String! " 
        ++ "Attempted types were: " 
        ++ aType ++ " and " ++ bType)

--Concatentates two Strings/Lists together.
doConcat :: EDState -> Either EDState String
doConcat state = 
    case (stack state) of 
        [] -> Right "Operator (++) error. Concatenation requires two operands; none provided!"  
        [x] -> Right "Operator (++) error. Concatenation requires two operands; only one provided!" 
        vals -> 
            let (state', a, b) = fsPop2 state
            in case (doConcat' a b) of 
                Left v -> Left (fsPush v state')
                Right err -> Right err

--Adds two values on stack if they can be added.
doAdd :: EDState -> Either EDState String
doAdd state = 
    let stck = (stack state)
    in case stck of 
        [] -> Right "Operator (+) error. Addition requires two operands; none provided!"
        [x] -> Right "Operator (+) error. Addition requires two operands; only one provided!" 
        vals ->  
            let (state', a, b) = fsPop2 state
                addRes = addVals a b
            in case addRes of 
                Left v -> Left (fsPush v state')
                Right err -> Right err

--Subtracts two values on stack if they can be subtracted.
doSub :: EDState -> Either EDState String
doSub state = 
    case (stack state) of 
        [] -> Right "Operator (-) error. Subtraction requires two operands; none provided!" 
        [x] -> Right "Operator (-) error. Subtraction requires two operands; only one provided!" 
        vals ->  
            let (state', b, a) = fsPop2 state
                subRes = subVals a b
            in case subRes of
                Left v -> Left (fsPush v state')
                Right err -> Right err

--Multiplies two values on top of stack if they can be multiplied.
doMul :: EDState -> Either EDState String
doMul state =  
    case (stack state) of 
        [] -> Right "Operator (*) error. Multiplication requires two operands; none provided!" 
        [x] -> Right "Operator (*) error. Multiplication requires two operands; only one provided!" 
        vals -> 
            let (state', a, b) = fsPop2 state
                mulRes = multVals a b
            in case mulRes of 
                Left v -> Left (fsPush v state')
                Right err -> Right err

--Divides two values on top of stack if they can be divided.
--Errors out if problem happens.
doDiv :: EDState -> Either EDState String
doDiv state =  
    case (stack state) of 
        [] -> Right "Operator (/) error. Division requires two operands; none provided!" 
        [x] -> Right "Operator (/) error. Division requires two operands; only one provided!" 
        vals -> 
            let (state', b, a) = fsPop2 state
                divRes = divideVals a b
            in case divRes of
                Left v -> Left (fsPush v state')
                Right err -> Right err

--Mods two values on top of stack if they can be modded.
--Errors out if problem happens.
doModulo :: EDState -> Either EDState String
doModulo state = 
    case (stack state) of 
        [] -> Right "Operator (%) error. Modulo requires two operands; none provided!" 
        [x] -> Right "Operator (%) error. Modulo requires two operands; only one provided!" 
        vals -> 
            let (state', b, a) = fsPop2 state
            in case (modVals a b) of
                Left v -> Left (fsPush v state')
                Right err -> Right err

--Swaps the top two values at the top of the stack.
doSwap :: EDState -> Either EDState String
doSwap state = 
    case (stack state) of 
        [] -> Left ( EDState{stack = [], fns = (fns state), vars = (vars state), frames = (frames state), heap = heap state, ops = ops state, ioOps = ioOps state} )
        [x] -> Left ( EDState{stack = [x], fns = (fns state), vars = (vars state), frames = (frames state), heap = heap state, ops = ops state, ioOps = ioOps state} )
        vals ->  
            let (state', b, a) = fsPop2 state
                state'' = fsPush a state' 
            in Left (fsPush b state'')

--Removes top value from stack.
doDrop :: EDState -> Either EDState String
doDrop state = 
    if null (stack state) 
    then 
        Left (state)
    else 
        let (state', _) = fsPop state
        in Left (state')

--Clears the entire stack to empty. 
-- Avoids having to type drop over and over again.
doDropStack :: EDState -> Either EDState String
doDropStack EDState{stack = _, fns = fs, vars = vs, frames = fms, heap = hp, ops = o, ioOps = io} = Left (EDState{stack = [], fns = fs, vars = vs, frames = fms, heap = hp, ops = o, ioOps = io})

--Rotates the top values on the stack.
--If there's 0 or 1 items, nothing happens.
--2 Items is identical to swap.
--3 items performs the rotation.
doRot :: EDState -> Either EDState String
doRot state = 
    case (stack state) of 
        [] ->  Left ( EDState{stack = [], fns = (fns state), vars = (vars state), frames = (frames state), heap = heap state, ops = ops state, ioOps = ioOps state} )
        [x] -> Left ( EDState{stack = [x], fns = (fns state), vars = (vars state), frames = (frames state), heap = heap state, ops = ops state, ioOps = ioOps state} )
        [x, y] -> Left ( EDState{stack = [y, x], fns = (fns state), vars = (vars state), frames = (frames state), heap = heap state, ops = ops state, ioOps = ioOps state} )
        vals -> 
            let (state', c, b, a) = fsPop3 state
                state'' = fsPush a state'
                state''' = fsPush c state''
            in Left (fsPush b state''') 

--Duplicates top element of stack or does nothing.
doDup :: EDState -> Either EDState String
doDup state = 
    case (stack state) of 
        [] -> Left state
        vals ->  
            let (_, top) = fsPop state
            in Left (fsPush top state)

--Checks equality of two elements at the top of the stack.
--Pushes true if they are equal and False if not.
doEqual :: EDState -> Either EDState String
doEqual state = 
    case (stack state) of 
        [] -> Right "Operator (==) error. Equality comparison requires two operands; none provided!" 
        [x] -> Right "Operator (==) error. Equality comparison requires two operands; only one provided!" 
        vals -> 
            let (state', b, a) = fsPop2 state
            in case (doEqual' a b) of 
                Left v -> Left (fsPush v state')
                Right err -> Right err

--Makes sure the types match and then performs the equality operation if so, 
-- otherwise returns error string.
doEqual' :: Value -> Value -> Either Value String
doEqual' (BigInteger a) (BigInteger b) = Left $ Boolean (a == b)
doEqual' (Integer a) (Integer b) = Left $ Boolean (a == b)
doEqual' (Float a) (Float b) = Left $ Boolean (a == b)
doEqual' (Double a) (Double b) = Left $ Boolean (a == b)
doEqual' (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Left $ Boolean ((al == bl) && (acs == bcs))
doEqual' (Char a) (Char b) = Left $ Boolean (a == b)
doEqual' (Boolean a) (Boolean b) = Left $ Boolean (a == b)
doEqual' (List {items = as, len = al}) (List {items = bs, len = bl}) = Left $ Boolean ((al == bl) && (as == bs))
doEqual' (Box bnA) (Box bnB) = Left $ Boolean $ bnA == bnB
doEqual' a b =
    let (aType, bType) = findTypeStrsForError a b  
    in Right ("Operator (==) error. Can't compare types that are not both types of" 
        ++ " BigIntegers, Integers, Floats, Doubles, String, Chars, Booleans, Lists, or Boxes! " 
        ++ "Attempted types were: " 
        ++ bType ++ " and " ++ aType)

--Checks inequality of two elements at the top of the stack.
--Pushes true if they are equal and False if not.
doNotEqual :: EDState -> Either EDState String
doNotEqual state = 
    case (stack state) of 
        [] -> Right "Operator (/=) error. Inequality comparison requires two operands; none provided!" 
        [x] -> Right "Operator (/=) error. Inequality comparison requires two operands; only one provided!" 
        vals ->  
            let (state', b, a) = fsPop2 state
            in case (doNotEqual' a b) of
                Left v -> Left (fsPush v state')
                Right err -> Right err 

--Makes sure the types match and then performs the inequality operation if so, 
-- otherwise errors out.
doNotEqual' :: Value -> Value -> Either Value String
doNotEqual' (BigInteger a) (BigInteger b) = Left $ Boolean (a /= b)
doNotEqual' (Integer a) (Integer b) = Left $ Boolean (a /= b)
doNotEqual' (Float a) (Float b) = Left $ Boolean (a /= b)
doNotEqual' (Double a) (Double b) = Left $ Boolean (a /= b)
doNotEqual' (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Left $ Boolean (acs /= bcs)
doNotEqual' (Char a) (Char b) = Left $ Boolean (a /= b)
doNotEqual' (Boolean a) (Boolean b) = Left $ Boolean (a /= b)
doNotEqual' (List {items = as, len = al}) (List {items = bs, len = bl}) = Left $ Boolean (as /= bs)
doNotEqual' (Box bnA) (Box bnB) = Left $ Boolean $ bnA /= bnB
doNotEqual' a b =
    let (aType, bType) = findTypeStrsForError a b  
    in Right ("Operator (/=) error. Can't compare types that are not both types of" 
        ++ " BigIntegers, Integers, Floats, Doubles, String, Chars, Booleans, Lists, or Boxes! " 
        ++ "Attempted types were: " 
        ++ bType ++ " and " ++ aType)

--Checks if second to top element is greater than top element of stack.
--Pushes True if true and false if not.
doGreaterThan :: EDState -> Either EDState String
doGreaterThan state =  
    case (stack state) of 
        [] -> Right "Operator (>) error. Greater than comparison requires two operands; none provided!" 
        [x] -> Right "Operator (>) error. Greater than comparison requires two operands; only one provided!" 
        vals -> 
            let (state', b, a) = fsPop2 state
            in case (doGreaterThan' b a) of 
                Left v -> Left (fsPush v state')
                Right err -> Right err
                
--Makes sure the types match and then performs the > operation if so, 
-- otherwise errors out.
doGreaterThan' :: Value -> Value -> Either Value String
doGreaterThan' (BigInteger a) (BigInteger b) = Left $ Boolean (a > b)
doGreaterThan' (Integer a) (Integer b) = Left $ Boolean (a > b)
doGreaterThan' (Float a) (Float b) = Left $ Boolean (a > b)
doGreaterThan' (Double a) (Double b) = Left $ Boolean (a > b)
doGreaterThan' (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Left $ Boolean (acs > bcs)
doGreaterThan' (Char a) (Char b) = Left $ Boolean (a > b)
doGreaterThan' (Boolean a) (Boolean b) = Left $ Boolean (a > b)
doGreaterThan' (List {items = as, len = al}) (List {items = bs, len = bl}) = Left $ Boolean (as > bs)
doGreaterThan' a b =
    let (aType, bType) = findTypeStrsForError a b  
    in Right ("Operator (>) error. Can't compare types that are not both types of" 
        ++ " BigIntegers, Integers, Floats, Doubles, String, Chars, Booleans, or Lists! " 
        ++ "Attempted types were: " 
        ++ aType ++ " and " ++ bType)

--Checks if second to top element is less than top element of stack.
--Pushes True if true and false if not.
doLessThan :: EDState -> Either EDState String
doLessThan state = 
    case (stack state) of 
        [] -> Right "Operator (<) error. Less than comparison requires two operands; none provided!" 
        [x] -> Right "Operator (<) error. Less than comparison requires two operands; only one provided!" 
        vals -> 
            let (state', b, a) = fsPop2 state
            in case (doLessThan' b a) of
                Left v -> Left (fsPush v state')
                Right err -> Right err

--Makes sure the types match and then performs the < operation if so, 
-- otherwise errors out.
doLessThan' :: Value -> Value -> Either Value String
doLessThan' (BigInteger a) (BigInteger b) = Left $ Boolean (a < b)
doLessThan' (Integer a) (Integer b) = Left $ Boolean (a < b)
doLessThan' (Float a) (Float b) = Left $ Boolean (a < b)
doLessThan' (Double a) (Double b) = Left $ Boolean (a < b)
doLessThan' (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Left $ Boolean (acs < bcs)
doLessThan' (Char a) (Char b) = Left $ Boolean (a < b)
doLessThan' (Boolean a) (Boolean b) = Left $ Boolean (a < b)
doLessThan' (List {items = as, len = al}) (List {items = bs, len = bl}) = Left $ Boolean (as < bs)
doLessThan' a b =
    let (aType, bType) = findTypeStrsForError a b  
    in Right ("Operator (<) error. Can't compare types that are not both types of" 
        ++ " BigIntegers, Integers, Floats, Doubles, String, Chars, Booleans, or Lists! " 
        ++ "Attempted types were: " 
        ++ aType ++ " and " ++ bType)

--Checks if second to top element is greater than equal to the top element of stack.
--Pushes True if true and false if not.
doGreaterThanEqualTo :: EDState -> Either EDState String
doGreaterThanEqualTo state = 
    case (stack state) of 
        [] -> Right "Operator (>=) error. Greater than equal to comparison requires two operands; none provided!" 
        [x] -> Right "Operator (>=) error. Greater than equal to comparison requires two operands; only one provided!" 
        vals -> 
            let (state', b, a) = fsPop2 state
            in case (doGreaterThanEqualTo' b a) of 
                Left v -> Left (fsPush v state')
                Right err -> Right err

--Makes sure the types match and then performs the >= operation if so, 
-- otherwise errors out.
doGreaterThanEqualTo' :: Value -> Value -> Either Value String
doGreaterThanEqualTo' (BigInteger a) (BigInteger b) = Left $ Boolean (a >= b)
doGreaterThanEqualTo' (Integer a) (Integer b) = Left $ Boolean (a >= b)
doGreaterThanEqualTo' (Float a) (Float b) = Left $ Boolean (a >= b)
doGreaterThanEqualTo' (Double a) (Double b) = Left $ Boolean (a >= b)
doGreaterThanEqualTo' (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Left $ Boolean (acs >= bcs)
doGreaterThanEqualTo' (Char a) (Char b) = Left $ Boolean (a >= b)
doGreaterThanEqualTo' (Boolean a) (Boolean b) = Left $ Boolean (a >= b)
doGreaterThanEqualTo' (List {items = as, len = al}) (List {items = bs, len = bl}) = Left $ Boolean (as >= bs)
doGreaterThanEqualTo' a b =
    let (aType, bType) = findTypeStrsForError a b  
    in Right ("Operator (>=) error. Can't compare types that are not both types of" 
        ++ " BigIntegers, Integers, Floats, Doubles, String, Chars, Booleans, or Lists! " 
        ++ "Attempted types were: " 
        ++ aType ++ " and " ++ bType)

--Checks if second to top element is less than equal to top element of stack.
--Pushes True if true and false if not.
doLessThanEqualTo :: EDState -> Either EDState String
doLessThanEqualTo state = 
    case (stack state) of 
        [] -> Right "Operator (<=) error. Less than equal to comparison requires two operands; none provided!" 
        [x] -> Right "Operator (<=) error. Less than equal to comparison requires two operands; only one provided!" 
        vals -> 
            let (state', b, a) = fsPop2 state
            in case (doLessThanEqualTo' b a) of
                Left v -> Left (fsPush v state')
                Right err -> Right err

--Makes sure the types match and then performs the <= operation if so, 
-- otherwise errors out.
doLessThanEqualTo' :: Value -> Value -> Either Value String
doLessThanEqualTo' (BigInteger a) (BigInteger b) = Left $ Boolean (a <= b)
doLessThanEqualTo' (Integer a) (Integer b) = Left $ Boolean (a <= b)
doLessThanEqualTo' (Float a) (Float b) = Left $ Boolean (a <= b)
doLessThanEqualTo' (Double a) (Double b) = Left $ Boolean (a <= b)
doLessThanEqualTo' (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Left $ Boolean (acs <= bcs)
doLessThanEqualTo' (Char a) (Char b) = Left $ Boolean (a <= b)
doLessThanEqualTo' (Boolean a) (Boolean b) = Left $ Boolean (a <= b)
doLessThanEqualTo' (List {items = as, len = al}) (List {items = bs, len = bl}) = Left $ Boolean (as <= bs)
doLessThanEqualTo' a b =
    let (aType, bType) = findTypeStrsForError a b  
    in Right ("Operator (<=) error. Can't compare types that are not both types of" 
        ++ " BigIntegers, Integers, Floats, Doubles, String, Chars, Booleans, or Lists! " 
        ++ "Attempted types were: " 
        ++ aType ++ " and " ++ bType)

--Performs logical AND function on top two elements of stack.
--If the operands are booleans, then AND is performed.
doAnd :: EDState -> Either EDState String
doAnd state = 
    case (stack state) of 
        [] -> Right "Operator (and) error. Logical AND requires two operands; none provided!"  
        [x] -> Right "Operator (and) error. Logical AND requires two operands; only one provided!" 
        vals -> 
            let (state', b, a) = fsPop2 state 
            in case (doAnd' a b) of 
                Left v -> Left (fsPush v state')
                Right err -> Right err

--Performs logical AND function on two operands and returns an updated EDState.
-- On failure, an error is returned.
doAnd' :: Value -> Value -> Either Value String
doAnd' (Boolean True) (Boolean True) = Left $ Boolean True
doAnd' (Boolean _) (Boolean _) = Left $ Boolean False
doAnd' a b =
    let (aType, bType) = findTypeStrsForError b a  
    in Right ("Operator (and) error. Can't logically AND two items that are not both types of" 
        ++ " Boolean! " 
        ++ "Attempted types were: " 
        ++ aType ++ " and " ++ bType)

--Performs logical OR function on top two elements of stack.
--If the operands are booleans, then OR is performed.
doOr :: EDState -> Either EDState String
doOr state = 
    case (stack state) of 
        [] -> Right "Operator (or) error. Logical OR requires two operands; none provided!"  
        [x] -> Right "Operator (or) error. Logical OR requires two operands; only one provided!" 
        vals -> 
            let (state', b, a) = fsPop2 state
            in case (doOr' a b) of
                Left v -> Left (fsPush v state')
                Right err -> Right err

--Performs logical OR function on two operands and returns an updated EDState.
-- On failure, an error is returned.
doOr' :: Value -> Value -> Either Value String
doOr' (Boolean False) (Boolean False) = Left $ Boolean False
doOr' (Boolean _) (Boolean _) = Left $ Boolean True
doOr' a b =
    let (aType, bType) = findTypeStrsForError b a  
    in Right ("Operator (or) error. Can't logically OR two items that are not both types of" 
        ++ " Boolean! " 
        ++ "Attempted types were: " 
        ++ aType ++ " and " ++ bType)

--Performs logical XOR function on top two elements of stack.
--If the operands are booleans, then XOR is performed.
doXor :: EDState -> Either EDState String
doXor state = 
    case (stack state) of 
        [] -> Right "Operator (xor) error. Logical XOR requires two operands; none provided!"  
        [x] -> Right "Operator (xor) error. Logical XOR requires two operands; only one provided!" 
        vals -> 
            let (state', b, a) = fsPop2 state 
            in case (doXor' a b) of
                Left v -> Left (fsPush v state')
                Right err -> Right err

--Performs logical XOR function on two operands and returns an updated EDState.
-- On failure, an error is thrown.
doXor' :: Value -> Value -> Either Value String
doXor' (Boolean True) (Boolean True) = Left $ Boolean False
doXor' (Boolean False) (Boolean False) = Left $ Boolean False
doXor' (Boolean _) (Boolean _) = Left $ Boolean True
doXor' a b =
    let (aType, bType) = findTypeStrsForError b a  
    in Right ("Operator (xor) error. Can't logically XOR two items that are not both types of" 
        ++ " Boolean! " 
        ++ "Attempted types were: " 
        ++ aType ++ " and " ++ bType)

--Performs the logical NOT operator on given boolean 
-- and throws errors when things go wrong.
doNot :: EDState -> Either EDState String
doNot state = 
    case (stack state) of 
        [] -> Right "Operator (not) error. Logical NOT operation requires one operand; none provided!" 
        vals -> 
            let (state', top) = fsPop state
            in case (doNot' top) of
                Left v -> Left (fsPush v state')
                Right err -> Right err 

--Performs negation if input value is of type boolean.
doNot' :: Value -> Either Value String
doNot' (Boolean b) = Left $ Boolean $ not b 
doNot' x = 
    let xType = chrs $ doQueryType' x
    in Right ("Operator (not) error. Can't logically NOT item that isn't type Boolean! " 
        ++ "Attempted type was: " ++ xType)

--Pushes an item to the end of a list on the stack.
doPush :: EDState -> Either EDState String
doPush state = 
    case (stack state) of 
        [] -> Right "Operator (push) error. Two operands required for push; none provided!" 
        [x] -> Right "Operator (push) error. Two operands required for push; only one provided!" 
        vals ->  
            let (state', list, val) = fsPop2 state
            in case (doPush' list val) of
                Left v -> Left (fsPush v state')
                Right err -> Right err

--Pushes item/char to list/string
doPush' :: Value -> Value -> Either Value String
doPush' (List {items = is, len = l}) valToPush = Left $ List {items = (M.insert l valToPush is), len = l + 1}
doPush' (String {chrs = cs, len = l}) (Char c) = Left $ String {chrs = cs ++ [c], len = l + 1}
doPush' a b = 
        let (aType, bType) = findTypeStrsForError a b 
        in Right ("Operator (push) error. Push operator needs a list/string and a value/char to be pushed. Attempted types: " 
            ++ aType ++ " and " ++ bType)

--Pops an item from the list or string and pushes it to the stack.
doPop :: EDState -> Either EDState String
doPop state = 
    case (stack state) of 
        [] -> Right "Operator (pop). error. Pop operator needs one operand; none provided!" 
        vals -> 
            let (state', list) = fsPop state
            in case (doPop' list) of 
                Left (list', Just v) -> 
                    let state'' = fsPush list' state'
                    in Left (fsPush v state'')
                Left (list', Nothing) -> Left (fsPush list' state')
                Right err -> Right err

--Pops item/char from list and returns new list with popped item if it exists.
doPop' :: Value -> Either (Value, Maybe Value) String
--Nothing happens if list is empty.
doPop' (List {items = is, len = 0}) = Left (List {items = is, len = 0}, Nothing)
--General list pop case.
doPop' (List {items = is, len = l}) = 
    let popped = case (M.lookup (l - 1) is) of 
            Just i -> i 
            Nothing -> error "Should never happen!!!"
        newLs = M.delete (l - 1) is
    in Left (List {items = newLs, len = l - 1}, Just popped)
--Empty string case.
doPop' (String {chrs = "", len = 0}) = Left (String {chrs = "", len = 0}, Nothing) 
--General string pop case.
doPop' (String {chrs = cs, len = l}) = 
    let newStr = String{chrs = init cs, len = l - 1}
        poppedChar = Char $ last cs
    in Left (newStr, Just poppedChar) 
doPop' x = 
    let xType = chrs $ doQueryType' x
    in Right ("Operator (pop) error. Pop operator needs a List/String to pop items on top of stack. Attempted type: "
        ++ xType)

--Pushes an item to the front of a list on the stack.
doFpush :: EDState -> Either EDState String
doFpush state = 
    case (stack state) of 
        [] -> Right "Operator (fpush) error. Two operands required for fpush; none provided!" 
        [x] -> Right "Operator (fpush) error. Two operands required for fpush; only one provided!" 
        vals -> 
            let (state', collection, val) = fsPop2 state
            in case (doFpush' collection val) of
                Left collection' -> Left (fsPush collection' state')
                Right err -> Right err

--Pushes item to front.
doFpush' :: Value -> Value -> Either Value String
doFpush' (List {items = is, len = l}) valToPush = Left $ doFpush'' List {items = is, len = l} 0 valToPush
doFpush' (String{chrs = cs, len = l}) (Char c) = Left String {chrs = (c : cs), len = l + 1}
doFpush' a b = 
    let (aType, bType) = findTypeStrsForError a b
    in Right ("Operator (fpush) error. Operator fpush needs List/String and a Value/Char to be pushed to front. Attempted Types: "
        ++ aType ++ " and " ++ bType) 

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
doFpop :: EDState -> Either EDState String
doFpop state = 
    case (stack state) of 
        [] -> Right "Operator (fpop) error. Needs one operand to work; none provided!" 
        vals -> 
            let (state', list) = fsPop state
            in case (doFpop' list) of 
                Left (list', Just v) -> 
                    let state'' = fsPush list' state'
                    in Left (fsPush v state'')
                Left (list', Nothing) -> Left (fsPush list' state')
                Right err -> Right err

--Pops item and pushes to stack if exists.
doFpop' :: Value -> Either (Value, Maybe Value) String

doFpop' List {items = is, len = 0} = Left (List{items = is, len = 0}, Nothing)

doFpop' List {items = is, len = l} =
    let popped = case (M.lookup 0 is) of
            Just i -> i
            Nothing -> error "Shouldn't EVER get here!"
        --Builds new list without front item which is why it starts at index 1 here.
        newLs = doFpop'' List{items = is, len = l} List{items = M.empty, len = 0} 1
    in Left (newLs, Just popped)

doFpop' String{chrs = "", len = 0} = Left (String{chrs = "", len = 0}, Nothing)

doFpop' String{chrs = cs, len = l} =
    let string' = String{chrs = tail cs, len = l - 1}
        popped = Char $ head cs
    in Left (string', Just popped)

doFpop' x = 
    let xType = chrs $ doQueryType' x
    in Right ("Operator (fpop) error. Popping from front requires a List/String to pop from. Attempted type: "
        ++ xType)

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
doIndex :: EDState -> Either EDState String
doIndex state = 
    case (stack state) of 
        [] -> Right "Operator (index) error. Two operands required for index; none provided!" 
        [x] -> Right "Operator (index) error. Two operands required for index; only one provided!" 
        vals ->  
            let (state', list, index) = fsPop2 state
            in case (doIndex' list index) of
                Left v -> 
                    let state'' = fsPush list state'
                    in Left (fsPush v state'')
                Right err -> Right err 

--Retrieves item at index in list or string.
doIndex' :: Value -> Value -> Either Value String

--Standard list index case.
doIndex' List{items = is, len = l} (Integer i) =
    case (M.lookup i is) of 
        Just el -> Left el
        Nothing -> Right ("Operator (index) error. Index " ++ (show i) ++ " out of valid range for List of size " ++ (show l) ++ "!")

--Standard string index case.
doIndex' String{chrs = cs, len = l} (Integer i) 
    |   (i > -1 && i < l) = Left $ Char $ cs !! i
    |   otherwise = Right ("Operator (index) error. Index " ++ (show i) ++ " out of valid range for String of size " ++ (show l) ++ "!") 

--Type error case.
doIndex' a b = 
    let (aType, bType) = findTypeStrsForError a b
    in Right ("Operator (index) error. Index operator needs a List/String and an index value of type Integer. Attempted types: "
        ++ aType ++ " and " ++ bType)

--Takes the length of a list or string at the top 
-- of the stack and pushes resulting length to top of stack.
doLength :: EDState -> Either EDState String
doLength state = 
    case (stack state) of 
        [] -> Right "Operator (length) error. Operand needed for length; none provided!" 
        vals -> 
            let top = (fsTop state)
            in case (doLength' top) of
                Left v -> Left (fsPush v state)
                Right err -> Right err

--Performs actual length function.
doLength' :: Value -> Either Value String
doLength' List{items = _, len = l} = Left $ Integer l
doLength' String {chrs = _, len = l} = Left $ Integer l
doLength' x = 
    let xType = chrs $ doQueryType' x
    in Right ("Operator (length) error. List/String type is needed for length operator to work. Attempted type: "
        ++ xType)

--Determines if the list or string at the top
-- of the stack is empty or not.
doIsEmpty :: EDState -> Either EDState String
doIsEmpty state = 
    case (stack state) of 
        [] -> Right "Operator (isEmpty) error. One operand needed; none provided!" 
        vals -> 
            let top = (fsTop state)
            in case (doIsEmpty' top) of 
                Left v -> Left (fsPush v state)
                Right err -> Right err

--Performs actual length function.
doIsEmpty' :: Value -> Either Value String
doIsEmpty' List{items = is, len = l} = Left $ Boolean $ l == 0
doIsEmpty' String{chrs = cs, len = l} = Left $ Boolean $ l == 0
doIsEmpty' Object{fields = fs} = Left $ Boolean $ M.null fs
doIsEmpty' x =
    let xType = chrs $ doQueryType' x 
    in Right ("Operator (isEmpty) error. This operator is only valid for types of List/String/Object. Attempted type: "
        ++ xType)

--Sets the string or list at the top of the stack to empty.
doClear :: EDState -> Either EDState String
doClear state = 
    case (stack state) of 
        [] -> Right "Operator (clear) error. One operand needed; none provided!" 
        vals ->  
            let (state', items) = fsPop state
            in case (doClear' items) of
                Left items' -> Left (fsPush items' state')
                Right err -> Right err

--Performs clear operation.
doClear' :: Value -> Either Value String
doClear' List{items = _, len = _} = Left List{items = M.empty, len = 0}
doClear' String{chrs = _, len = _} = Left String{chrs = "", len = 0} 
doClear' Object{fields = _} = Left Object{fields = M.empty}
doClear' x =
    let xType = chrs $ doQueryType' x
    in Right ("Operator (clear) error. Only type List/String/Object is valid for clear. Attempted type: "
        ++ xType)

--Used to turn a value of one type into another.
doCast :: EDState -> Either EDState String
doCast state = 
    case (stack state) of 
        [] -> Right "Operator (cast) error. Two operands required for cast; none provided!" 
        [x] -> Right "Operator (cast) error. Two operands required for cast; only one provided!" 
        vals ->  
            let (state', val, castType) = fsPop2 state
            in case (doCast' val castType) of 
                Left v -> Left (fsPush v state')
                Right err -> Right err 

--Performs the actual cast operation.
doCast' :: Value -> Value -> Either Value String

doCast' (Boolean b) String{chrs = "Integer", len = _} = Left $ Integer $ if b then 1 else 0
doCast' (Boolean b) String{chrs = "BigInteger", len = _} = Left $ BigInteger $ if b then 1 else 0
doCast' (Boolean b) String{chrs = "String", len = _} = Left $ let boolStr = show b in String {chrs = boolStr, len = length boolStr}
doCast' (Boolean b) String{chrs = "Boolean", len = _} = Left $ Boolean b --Do nothing since it's already boolean.

doCast' (BigInteger n) String{chrs = "String", len = _} = Left $ let bigIntStr = show n in String{chrs = bigIntStr, len = length bigIntStr}
doCast' (BigInteger n) String{chrs = "Integer", len = _} = Left $ Integer (fromIntegral n :: Int)
doCast' (BigInteger n) String{chrs = "BigInteger", len = _} = Left $ BigInteger n --Do nothing case since it's already bigint.
doCast' (BigInteger n) String{chrs = "Float", len = _} = Left $ Float (fromIntegral n :: Float)
doCast' (BigInteger n) String{chrs = "Double", len = _} = Left $ Double (fromIntegral n :: Double)
doCast' (BigInteger n) String{chrs = "Char", len = _} =
    let nInt = fromIntegral n :: Int
    in if validIntToChar nInt
        then Left $ Char $ chr nInt
        else Right ("Operator (cast) error. " 
            ++ "Failed to convert type BigInteger to Char." ++ 
            " Try making sure the Integer is in the UTF-8 numerical range."
            ++ " Given value: " ++ (show n) ++ " valid numbers are " ++ (show $ ord minBound) ++ " to " ++ (show $ ord maxBound) ++ ".") 

doCast' (Integer n) String{chrs = "String", len = _} = Left $ let intStr = show n in String {chrs = intStr, len = length intStr}
doCast' (Integer n) String{chrs = "Integer", len = _} = Left $ Integer n --Do nothing case.
doCast' (Integer n) String{chrs = "BigInteger", len = _} = Left $ BigInteger (fromIntegral n :: Integer)
doCast' (Integer n) String{chrs = "Float", len = _} = Left $ Float (fromIntegral n :: Float)
doCast' (Integer n) String{chrs = "Double", len = _} = Left $ Double (fromIntegral n :: Double)
doCast' (Integer n) String{chrs = "Char", len = _} =
    if validIntToChar n 
        then Left $ Char $ chr n
        else Right ("Operator (cast) error. " 
            ++ "Failed to convert type Integer to Char." ++ 
            " Try making sure the Integer is in the UTF-8 numerical range. "
            ++ "Given value: " ++ (show n) ++ " valid numbers are " ++ (show $ ord minBound) ++ " to " ++ (show $ ord maxBound) ++ ".")

doCast' (Float n) String{chrs = "String", len = _} = Left $ let floatStr = show n in String{chrs = floatStr, len = length floatStr}
doCast' (Float n) String{chrs = "Integer", len = _} = Left $ Integer $ truncate n
doCast' (Float n) String{chrs = "BigInteger", len = _} = Left $ BigInteger (floor n :: Integer)
doCast' (Float n) String{chrs = "Float", len = _} = Left $ Float n --Do nothing!
doCast' (Float n) String{chrs = "Double", len = _} = Left $ Double (realToFrac n :: Double) --Has errors that crop up!

doCast' (Double n) String{chrs = "String", len = _} = Left $ let dbStr = show n in String{chrs = dbStr, len = length dbStr}
doCast' (Double n) String{chrs = "Integer", len = _} = Left $ Integer (truncate n)
doCast' (Double n) String{chrs = "BigInteger", len = _} = Left $ BigInteger (floor n :: Integer)
doCast' (Double n) String{chrs = "Float", len = _} = Left $ Float (realToFrac n :: Float)
doCast' (Double n) String{chrs = "Double", len = _} = Left $ Double n --Do nothing here!

doCast' String{chrs = cs, len = l} String{chrs = "String", len = _} = Left $ String{chrs = cs, len = l} --Do nothing since it's string to string.

doCast' (Char c) String{chrs = "String", len = _} = Left $ let cStr = [c] in String{chrs = cStr, len = length cStr}
doCast' (Char c) String{chrs = "Integer", len = _} = Left $ Integer $ ord c
doCast' (Char c) String{chrs = "BigInteger", len = _} = Left $ BigInteger (fromIntegral (ord c) :: Integer)

doCast' String{chrs = cs, len = l} String{chrs = "Integer", len = _} =
    case (readMaybe cs :: Maybe Int) of 
        Just v -> Left $ Integer v
        Nothing -> Right ("Operator (cast) error. Failed to convert String \"" ++ cs ++ "\" to type Integer.")

doCast' String{chrs = cs, len = l} String{chrs = "BigInteger", len = _} =
    case (readMaybe cs :: Maybe Integer) of 
        Just v -> Left $ BigInteger v
        Nothing -> Right ("Operator (cast) error.\nFailed to convert String \"" ++ cs ++ "\" to type BigInteger.")

doCast' String{chrs = cs, len = l} String{chrs = "Float", len = _} =
    case (readMaybe cs :: Maybe Float) of 
        Just v -> Left $ Float v
        Nothing -> Right ("Operator (cast) error.\nFailed to convert String \"" ++ cs ++ "\" to type Float.")

doCast' String{chrs = cs, len = l} String{chrs = "Double", len = _} =
    case (readMaybe cs :: Maybe Double) of
        Just v -> Left $ Double v
        Nothing -> Right ("Operator (cast) error.\nFailed to convert String \"" ++ cs ++ "\" to type Double.")

doCast' List{items = is, len = l} String{chrs = "String", len = _} =
    let listStr = "[" ++ (printList List{items = is, len = l} "" 0 False) ++ "]"
        listStrLen = length listStr
    in Left String{chrs = listStr, len = listStrLen}

doCast' Object{fields = fs} String{chrs = "String", len = _} =
    let objStr = "{" ++ (printObj (M.toList fs) "") ++ "}"
        objStrLen = length objStr
    in Left String{chrs = objStr, len = objStrLen}

--Box casting cases.
doCast' (Box bn) String{chrs = "String", len = _} =
    if (bn == (-1))
        then Left $ String{chrs = "Box NULL", len = length "Box NULL"}
        else let boxStr = "Box " ++ (show bn) in Left String{chrs = boxStr, len = length boxStr}
doCast' (Box bn) String{chrs = "Integer", len = _} = Left $ Integer bn
--Truthy vs falsy value, kinda useless but why not.
doCast' (Box bn) String{chrs = "Boolean", len = _} = Left $ Boolean $ bn /= (-1)

--Casting failure cases.
doCast' a String{chrs = typeCastStr, len = _} =
    let aType = chrs $ doQueryType' a 
    in Right ("Operator (cast) error. Invalid casting configuration given! Tried to cast "
        ++ aType ++ " to type " ++ typeCastStr)
doCast' a b =
    let (aType, bType) = findTypeStrsForError a b
    in Right("Operator (cast) error. Types of Value and String required for cast to occur. Attempted types: "
        ++ aType ++ " and " ++ bType)

--Determines if the number is 
-- in the valid UTF-8 character number range for casting.
validIntToChar :: Int -> Bool
validIntToChar num = (num >= (ord minBound)) && (num <= (ord maxBound))

--Prints top element of stack. This element must be a string or it freaks out.
doPrintLine :: EDState -> IO EDState
doPrintLine state = 
    if (null $ stack state) 
        then throwError "Operator (printLine) error. Can't print from empty stack!" state
        else 
            case (fsTop state) of 
                String {chrs = cs, len = l} -> putStrLn cs >> return state
                x ->
                    let xType = chrs $ doQueryType' x
                    in throwError ("Operator (printLine) error. Top of stack needs to be type String! Attempted type: "
                        ++ xType) state 

--Writes a string to stdout without adding a newline automatically to the end.
doPrint :: EDState -> IO EDState
doPrint state = 
    if (null $ stack state)
        then 
            throwError "Operator (print) error. One operand needed for print; none provided!" state
        else 
            case (fsTop state) of 
                String{chrs = cs, len = l} -> putStr cs >> return state 
                x -> 
                    let xType = chrs $ doQueryType' x
                    in throwError ("Operator (print) error. " 
                        ++ "Top of stack needs to be a String to be printed! "
                        ++ "Attempted type: " ++ xType) state

--Reads a line from stdin, and pushes it onto stack.
doReadLine :: EDState -> IO EDState
doReadLine state = getLine >>= (\input -> return (fsPush (String{chrs = input, len = length input}) state))

--Reads a multi-line string from stdin.
doRead :: EDState -> IO EDState
doRead state = do 
    captured <- doRead' ""
    return (fsPush (String{chrs = captured, len = length captured}) state)

--Prints a desired error to stdout.
doPrintError :: EDState -> IO EDState
doPrintError state = 
    case (stack state) of 
        ((String{chrs = err, len = _}):xs) -> throwError err state
        (x:xs) -> let xType = chrs $ doQueryType' x in throwError ("Operator (printError) error. \
            \String needed on top of stack for error to print! Attempted type: " ++ xType) state
        [] -> throwError "Operator (printError) error. One operand required; none provided!" state

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
doPrintChar state = 
    if (null $ stack state)
        then 
            throwError "Operator (printChar) error. Can't print Char from empty stack!" state
        else 
            case (fsTop state) of 
                Char c -> putChar c >> return state 
                x -> 
                    let xType = chrs $ doQueryType' x
                    in throwError ("Operator (printChar) error. " 
                        ++ "Top of stack must be of type Char! Attempted type: " 
                        ++ xType) state
                
--Reads a Char from stdin and pushes it to the stack.
doReadChar :: EDState -> IO EDState 
doReadChar state = getChar >>= (\inChar -> return (fsPush (Char inChar) state))

--Determines if a character at the top 
-- of the stack is a whitespace character, 
-- pushes true if yes and false if no.
doIsWhite :: EDState -> Either EDState String
doIsWhite state = 
    case (stack state) of 
        [] -> Right "Operator (isWhitespace) error. Operand on stack needed; none provided!" 
        vals -> case (head vals) of 
            Char c -> Left (fsPush (Boolean (isSpace c)) state)
            x -> 
                let xType = chrs $ doQueryType' x
                in Right ("Operator (isWhitespace) error. Type on stack top needs to be of type Char. Attempted type: " ++ xType) 

--Determines if a list, string, 
-- or object contains a value, char, or field, respectively.
doContains :: EDState -> Either EDState String
doContains state = 
    case (stack state) of 
        [] -> Right "Operator (contains) error. Two operands on stack needed; none provided!" 
        [x] -> Right "Operator (contains) error. Two operands on stack needed; only one provided!" 
        vals -> 
            let (state', secondToTop, top) = fsPop2 state
            in case (top, secondToTop) of 
                (v, List {items = is, len = _}) -> Left (fsPush (Boolean $ v `elem` is) state)
                (Char c, String {chrs = cs, len = _}) -> Left (fsPush (Boolean $ c `elem` cs) state)
                (String{chrs = name, len = _}, Object{fields = fs}) -> 
                    let contains = case (M.lookup name fs) of 
                            Just _ -> True 
                            Nothing -> False
                    in Left (fsPush (Boolean contains) state)
                (a, b) -> 
                    let (aType, bType) = findTypeStrsForError b a
                    in Right ("Operator (contains) error." 
                        ++ " First pushed element must be List/String/Object " 
                        ++ "and second item needs to be Value, Char, or String, respectively. "
                        ++ "Attempted types: " ++ aType ++ " and " ++ bType ) 

--Changes an item at a given index in a list to a new item on the stack.
doChangeItemAt :: EDState -> Either EDState String
doChangeItemAt state = 
    case (stack state) of 
        [] -> Right "Operator (changeItemAt) error. Three operands needed; none provided!" 
        [x] -> Right "Operator (changeItemAt) error. Three operands needed; only one provided!" 
        [x, y] -> Right "Operator (changeItemAt) error. Three operands needed; only two provided!" 
        vals ->  
            let (state', chngLs, chngItem, index) = fsPop3 state
            in case (chngLs, chngItem, index) of 
                (List {items = is, len = l}, v, Integer i) -> 
                    if (i > -1 && i < l) 
                        then 
                            Left (fsPush ( List { items = M.insert i v is, len = l } ) state')
                        else 
                            Right ("Operator (changeItemAt) error. Index " 
                                ++ (show i) ++ " out of range for list of size " 
                                ++ (show l) 
                                ++ "!") 
                (a, b, c) -> 
                    let (aType, bType, cType) = (chrs $ doQueryType' a, chrs $ doQueryType' b, chrs $ doQueryType' c)
                    in Right ("Operator (changeItemAt) error."
                    ++ " Top three items of stack need to be of type: "
                    ++ "List Value Integer (ordered from bottom to top). Attempted types: "
                    ++ aType ++ ", " ++ bType ++ ", and " ++ cType) 

--Raises one Float or Double to another Float or Double 
--and returns as such, consuming the original two numbers.
doPow :: EDState -> Either EDState String
doPow state =  
    case (stack state) of 
        [] -> Right "Operator (pow) error. Two operands needed; none provided!" 
        [x] -> Right "Operator (pow) error. Two operands needed; only one provided!" 
        vals ->  
            let (state', base, expnt) = fsPop2 state
            in case (base, expnt) of 
                (Float bs, Float ex) -> Left (fsPush (Float (bs ** ex)) state')
                (Double bs, Double ex) -> Left (fsPush (Double (bs ** ex)) state')
                (a, b) ->
                    let (aType, bType) = findTypeStrsForError a b
                    in Right ("Operator (pow) error. Operands need to be both of type Float or Double. Attempted types: " 
                        ++ aType ++ " and " ++ bType)  

--Adds a field to a given object.
doAddField :: EDState -> Either EDState String
doAddField state =
    case (stack state) of 
        [] -> Right "Operator (addField) error. Three operands needed; none provided!" 
        [x] -> Right "Operator (addField) error. Three operands needed; only one provided!" 
        [x, y] -> Right "Operator (addField) error. Three operands needed; only two provided!" 
        vals ->  
            let (state', obj, fieldName, fieldVal) = fsPop3 state
            in case (doAddField' obj fieldName fieldVal) of 
                Left obj' -> Left (fsPush obj' state')
                Right err -> Right err 

--Adds field to object.
doAddField' :: Value -> Value -> Value -> Either Value String

doAddField' Object{fields = fs} String{chrs = name, len = _} val =
    case (M.lookup name fs) of
        Just i -> Right ("Operator (addField) error. Field " 
            ++ name 
            ++ " already exists in given object!")
        Nothing -> Left $ Object{fields = M.insert name val fs}

doAddField' a b c =
    let (aType, bType, cType) = (chrs $ doQueryType' a, chrs $ doQueryType' b, chrs $ doQueryType' c)
    in Right ("Operator (addField) error. Operands need to be Object String Value. Attempted types: "
        ++ aType ++ ", " ++ bType ++ ", and " ++ cType)

--Removes a field from a given object. Does nothing if the field doesn't exist.
doRemoveField :: EDState -> Either EDState String
doRemoveField state = 
    case (stack state) of 
        [] -> Right "Operator (removeField) error. Two operands needed; none provided!" 
        [x] -> Right "Operator (removeField) error. Two operands needed; only one provided!" 
        vals ->  
            let (state', obj, removalKey) = fsPop2 state
            in case (obj, removalKey) of 
                (Object{fields = fs}, String{chrs = name, len = l}) -> 
                    case (M.lookup name fs) of 
                            Just i -> Left (fsPush  Object{fields = M.delete name fs} state')
                            Nothing -> Right ("Operator (removeField) error. Field " ++ name ++ " doesn't exist in given object!") 
                (a, b) -> 
                    let (aType, bType) = findTypeStrsForError a b
                    in Right ("Operator (removeField) error. Operands need to be of type Object and String. Attempted types: "
                        ++ aType ++ " and " ++ bType)

--Grabs the value of a field in an object or throws an error if it doesn't exist.
doGetField :: EDState -> Either EDState String
doGetField state = 
    case (stack state) of 
        [] -> Right "Operator (getField) error. Two operands needed; none provided!" 
        [x] -> Right "Operator (getField) error. Two operands needed; only one provided!" 
        vals ->  
            let (state', obj, findKey) = fsPop2 state 
            in case (obj, findKey) of 
                (Object{fields = fs}, String{chrs = name, len = l}) -> 
                    case (M.lookup name fs) of 
                        Just i -> Left (fsPush i (fsPush obj state'))
                        Nothing -> Right ("Operator (getField) error. Field " ++ name ++ " doesn't exist in given object!") 

                (a, b) ->
                    let (aType, bType) = findTypeStrsForError a b
                    in Right ("Operator (getField) error. Operands need to be type Object and String. Attempted types: "
                        ++ aType ++ " and " ++ bType)

--Mutates the value of a field in the object assuming the field exists 
-- and the types match for the old and new values.
doMutateField :: EDState -> Either EDState String
doMutateField state = 
    case (stack state) of 
        [] -> Right "Operator (mutateField) error. Three operands needed; none provided!" 
        [x] -> Right "Operator (mutateField) error. Three operands needed; only one provided!" 
        [x, y] -> Right "Operator (mutateField) error. Three operands needed; only two provided!" 
        vals ->
            let (state', obj, mutKey, newVal) = fsPop3 state 
            in case (obj, mutKey) of 
                (Object{fields = fs}, String{chrs = name, len = l}) -> 
                    case (M.lookup name fs) of 
                        Just i -> if (compareTypesForMut i newVal) 
                            then Left $ fsPush Object{fields = (M.insert name newVal fs)} state' 
                            else Right ("Operator (mutateField) error. New value is of type: "
                                ++ (chrs $ doQueryType' newVal) ++ " which doesn't match field " 
                                ++ name ++ " of type " ++ (chrs $ doQueryType' i) ++ ". The types must match for consistency!")

                        Nothing -> Right ("Operator (mutateField) error. Field " ++ name ++ " doesn't exist in given object!") 
                (a, b) -> 
                    let (aType, bType) = findTypeStrsForError a b
                    in Right ("Operator (mutateField) error. Operands need to be of type Object String Value. Attempted types: "
                        ++ aType ++ ", " ++ bType ++ ", and " ++ (chrs $ doQueryType' newVal))

--Reads in the contents of a file to a string.
doReadFile :: EDState -> IO EDState 
doReadFile state = 
    case (stack state) of 
        [] -> throwError "Operator (readFile) error. One operand needed; none provided!" state
        vals ->  
            let (state', fileName) = fsPop state 
            in case (fileName) of
                    --(In Mr Bean voice) Magic (chuckles snortily) 
                    (String{chrs = cs, len = l}) -> do 
                        openResult <- tryIOError $ openFile cs ReadMode
                        case openResult of
                            Left e -> throwError ("Operator (readFile) error. Failed to open file " ++ cs ++ " because: " ++ (show e)) state'
                            Right handle -> do
                                fileStr <- hGetContents handle
                                let state'' = fsPush (String{chrs = fileStr, len = length fileStr}) state'
                                fileStr `deepseq` (hClose handle >> return (state''))
                    x -> 
                        let xType = chrs $ doQueryType' x
                        in throwError ("Operator (readFile) error. Operand needs to be of type String. Attempted type: "
                            ++ xType) state

--Writes the desired string to a given file name.
doWriteFile :: EDState -> IO EDState
doWriteFile state = 
    case (stack state) of 
        [] -> throwError "Operator (writeFile) error. Two operands needed; none provided!" state
        [x] -> throwError "Operator (writeFile) error. Two operands needed; only one provided!" state
        vals ->  
            let (state', fileName, writeContents) = fsPop2 state
            in case (fileName, writeContents) of 
                (String{chrs = name, len = _}, String{chrs = contents, len = l}) -> do 
                    openResult <- tryIOError $ openFile name WriteMode
                    case openResult of
                        Left err -> throwError ("Operator (writeFile) error. Failed to open file " ++ name ++ " to write because: " ++ (show err)) state'
                        Right handle -> do
                            hPutStr handle contents
                            hClose handle 
                            return (state')
                (a, b) -> 
                    let (aType, bType) = findTypeStrsForError a b 
                    in throwError ("Operator (writeFile) error. Operands need to be of type String and String. Attempted types: "
                        ++ aType ++ " and " ++ bType) state' 

--Determines the type of an item on the stack.
doQueryType :: EDState -> Either EDState String
doQueryType state =  
    case (stack state) of 
        [] -> Right "Operator (queryType) error. One operand needed; none provided!" 
        vals -> 
            let (state', val) = fsPop state
            in Left (fsPush (doQueryType' val) state)

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
doQueryType' (Box _) = String{chrs = "Box", len = length "Box"}

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

doBitOr :: EDState -> Either EDState String
doBitOr state = 
    let grabFirstInTriple = \(x, _, _) -> x
    in case (stack state) of 
        [] -> Right "Operator (bitOr) error. Two operands needed; none provided!" 
        [x] -> Right "Operator (bitOr) error. Two operands needed; only one provided!" 
        (Integer x1):(Integer x2):xs -> Left $ fsPush (Integer (x1 .|. x2)) (grabFirstInTriple $ fsPop2 state)
        (BigInteger x1):(BigInteger x2):xs -> Left $ fsPush (BigInteger (x1 .|. x2)) (grabFirstInTriple $ fsPop2 state)
        x1:x2:xs -> let (x1Type, x2Type) = findTypeStrsForError x1 x2 ; in
            Right ("Operator (bitOr) error. Bitwise OR requires two operands \
                \with matching types Integer or BigInteger! Attempted types: " ++ x2Type ++ " and " ++ x1Type)

doBitAnd :: EDState -> Either EDState String
doBitAnd state = 
    let grabFirstInTriple = \(x, _, _) -> x
    in case (stack state) of 
        [] -> Right "Operator (bitAnd) error. Two operands needed; none provided!" 
        [x] -> Right "Operator (bitAnd) error. Two operands needed; only one provided!" 
        (Integer x1):(Integer x2):xs -> Left $ fsPush (Integer (x1 .&. x2)) (grabFirstInTriple $ fsPop2 state)
        (BigInteger x1):(BigInteger x2):xs -> Left $ fsPush (BigInteger (x1 .&. x2)) (grabFirstInTriple $ fsPop2 state)
        x1:x2:xs -> let (x1Type, x2Type) = findTypeStrsForError x1 x2 ; in
            Right ("Operator (bitAnd) error. Bitwise AND requires two operands \
                \with matching types Integer or BigInteger! Attempted types: " ++ x2Type ++ " and " ++ x1Type)

doBitXor :: EDState -> Either EDState String
doBitXor state = 
    let grabFirstInTriple = \(x, _, _) -> x
    in case (stack state) of 
        [] -> Right "Operator (bitXor) error. Two operands needed; none provided!" 
        [x] -> Right "Operator (bitXor) error. Two operands needed; only one provided!" 
        (Integer x1):(Integer x2):xs -> Left $ fsPush (Integer (x1 `xor` x2)) (grabFirstInTriple $ fsPop2 state)
        (BigInteger x1):(BigInteger x2):xs -> Left $ fsPush (BigInteger (x1 `xor` x2)) (grabFirstInTriple $ fsPop2 state)
        x1:x2:xs -> let (x1Type, x2Type) = findTypeStrsForError x1 x2 ; in
            Right ("Operator (bitXor) error. Bitwise XOR requires two operands \
                \with matching types Integer or BigInteger! Attempted types: " ++ x2Type ++ " and " ++ x1Type) 

doBitNot :: EDState -> Either EDState String
doBitNot state =
    case (stack state) of
        [] -> Right "Operator (bitNot) error. One operand needed; none provided!" 
        (Integer x):xs -> Left $ fsPush (Integer (complement x)) (fst $ fsPop state)
        (BigInteger x):xs -> Left $ fsPush (BigInteger (complement x)) (fst $ fsPop state)
        x:xs -> let xType = chrs $ doQueryType' x ; 
            in Right ("Operator (bitNot) error. Bitwise NOT requires one operand of type Integer or BigInteger! Attempted type: " ++ xType)  

doBitShift :: EDState -> Either EDState String
doBitShift state =
    let grabFirstInTriple = \(x, _, _) -> x
    in case (stack state) of
        [] -> Right "Operator (bitShift) error. Two operands needed; none provided!" 
        [x] -> Right "Operator (bitShift) error. Two operands needed; only one provided!" 
        (Integer shiftAmount):(Integer x):xs -> Left $ fsPush (Integer $ shift x shiftAmount) (grabFirstInTriple $ fsPop2 state)
        (Integer shiftAmount):(BigInteger x):xs -> Left $ fsPush (BigInteger $ shift x shiftAmount) (grabFirstInTriple $ fsPop2 state)
        x1:x2:xs -> let (x1Type, x2Type) = findTypeStrsForError x1 x2 ; 
            in Right ("Operator (bitShift) error. Top of stack must be type Integer and second \
                \to top must be either type Integer or BigInteger! \
                \Valid types: Integer/BigInteger Integer. Attempted types: " ++ x2Type ++ " and " ++ x1Type) 

createOpsHashes :: (HM.HashMap String (EDState -> Either EDState String), HM.HashMap String (EDState -> IO EDState))
createOpsHashes = 
    let opsList = 
            [
                --Basic arithmetic ops.
                ("+", doAdd),
                ("-", doSub),
                ("*", doMul),
                ("/", doDiv),
                ("%", doModulo),
                ("pow", doPow),

                --Basic stack operators.
                ("swap", doSwap),
                ("drop", doDrop),
                ("dropStack", doDropStack),
                ("rot", doRot),
                ("dup", doDup),

                --Basic comparison operators.
                ("==", doEqual),
                ("/=", doNotEqual),
                (">", doGreaterThan),
                ("<", doLessThan),
                (">=", doGreaterThanEqualTo),
                ("<=", doLessThanEqualTo),

                ("++", doConcat),

                --Basic logical operators.
                ("and", doAnd),
                ("or", doOr),
                ("xor", doXor),
                ("not", doNot),

                --Basic list operations.
                ("push", doPush),
                ("p", doPush),
                ("pop", doPop),
                ("po", doPop),
                ("fpush", doFpush),
                ("fp", doFpush),
                ("fpop", doFpop),
                ("fpo", doFpop),
                ("index", doIndex),
                ("length", doLength),
                ("len", doLength),
                ("isEmpty", doIsEmpty),
                ("clear", doClear),
                ("contains", doContains),
                ("changeItemAt", doChangeItemAt),
                
                ("isWhitespace", doIsWhite),
                
                --Type stuff.
                ("cast", doCast),
                ("queryType", doQueryType),

                --Object operators.
                ("addField", doAddField),
                ("removeField", doRemoveField),
                ("getField", doGetField),
                ("mutateField", doMutateField),

                --Bitwise operators.
                ("bitOr", doBitOr),
                ("bitAnd", doBitAnd),
                ("bitXor", doBitXor),
                ("bitNot", doBitNot),
                ("bitShift", doBitShift)
            ]
        ioOpsList = 
            [
                --IO stuff.
                ("printLine", doPrintLine),
                ("readLine", doReadLine),
                ("printChar", doPrintChar),
                ("readChar", doReadChar),
                ("print", doPrint),
                ("read", doRead),
                ("printError", doPrintError),
                ("debugPrintStack", doDebugPrintStack),

                --File IO Operations.
                ("readFile", doReadFile),
                ("writeFile", doWriteFile)
            ]
    in (HM.fromList opsList, HM.fromList ioOpsList)

astNodeToString :: AstNode -> String
astNodeToString (Terminal (Word w)) = w

--Performs a lookup through all stack frames to find desired variable name.
--If found, returns it, else returns Nothing.
getLoc :: [M.Map String Value] -> String -> Maybe Value
getLoc [] name = Nothing
getLoc (f:fs) name = 
    let look = M.lookup name f 
    in case look of 
        Just v -> Just v
        Nothing -> getLoc fs name  

--Recursively builds a new list of stack frames with the mutated variable in it.
--Altered to now linearly do this!
updateFrames :: [M.Map String Value] -> [M.Map String Value] -> Value -> String -> Bool -> [M.Map String Value]
updateFrames [] acc mutVal name hasUpdated = reverse acc
updateFrames (f:fs) acc mutVal name hasUpdated = 
    if hasUpdated
        then 
            updateFrames fs (f : acc) mutVal name hasUpdated
        else 
            case (M.lookup name f) of
                Just _ -> 
                    let f' = M.insert name mutVal f
                    in updateFrames fs (f' : acc) mutVal name True 
                Nothing -> updateFrames fs (f : acc) mutVal name hasUpdated

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
compareTypesForMut (Box _) (Box _) = True
compareTypesForMut _ _ = False

--Used to add a stack frame when the scope increases. 
addFrame :: EDState -> EDState
addFrame state = EDState {stack = (stack state), fns = (fns state), vars = (vars state), frames = ((M.empty):(frames state)), heap = heap state, ops = ops state, ioOps = ioOps state}

--Removes stack frame if not at global scope.
removeFrame :: EDState -> EDState
removeFrame EDState{stack = s, fns = f, vars = v, frames = [x], heap = hp, ops = o, ioOps = io} = 
    EDState{stack = s, fns = f, vars = v, frames = [x], heap = hp, ops = o, ioOps = io}
removeFrame EDState{stack = s, fns = f, vars = v, frames = (x:xs), heap = hp, ops = o, ioOps = io} =
    EDState{stack = s, fns = f, vars = v, frames = (xs), heap = hp, ops = o, ioOps = io}

--Determines if desired box number is valid on the heap.
-- Returns a value if the box number is valid and returns an error string if not.
validateBox :: Heap -> Int -> Either Value String
validateBox Heap{freeList = fl, h = hp, heapSize = s} bn = 
    if (bn == (-1)) then Right "Operator (box) error. Box interaction with a NULL Box occuring! Can't operate using a NULL Box!"
    else if (bn > (-1) && bn < s)
        then
            case (M.lookup bn fl) of
                Just _ -> Right ("Operator (box) error. Box number " ++ (show bn) ++ " isn't a valid Box number! Box " ++ (show bn) ++ " has been free'd!")
                Nothing -> 
                    case (M.lookup bn hp) of
                        Just v -> Left v
                        Nothing -> Right ("Operator (box) error. Box number " ++ (show bn) ++ " doesn't exist in the heap!")
        else
            Right ("Operator (box) error. Box number " ++ (show bn) ++ " isn't a valid Box number! Box number out of range of heap of size " ++ (show s))

--Runs through the code and executes all nodes of the AST.
doNode :: AstNode -> EDState -> IO EDState

--Attempt on Error doNode case where it tries to run code in attempt branch 
-- and instead runs the code in on Error if there's a problem.
doNode (Expression((AttErr{attempt = att, onError = err}):rest)) state = (catch (doNode att (addFrame state)) handler) >>= (\state' -> doNode (Expression rest) state') 
    where 
        handler :: GeneralException -> IO EDState
        handler (GeneralException msg) = 
            let state' = fsPush (String {chrs = msg, len = length msg}) (addFrame state)
            in doNode err state'

--Pattern matches TempStackChange block. In this block, the code inside it runs but importantly 
-- without a stack change like with other operators like this.
doNode (Expression ((TempStackChange runBlock):rest)) state =
    (doNode runBlock (addFrame state)) >>= (\state' -> doNode (Expression rest) (EDState{stack = stack state, fns = fns state', vars = vars state', 
            frames = frames state', heap = heap state', ops = ops state', ioOps = ioOps state'}))

--Parses box command.
doNode (Expression ((BoxOp cmd):rest)) state =
    case (astNodeToString cmd) of
        "make" -> 
            if (null $ stack state)
                then
                    throwError "Operator (box make) error. Can't make a box with no data on stack to give it!" state
                else
                    let (Heap{freeList = fl, h = hp, heapSize = hs}) = heap state
                        (state', v) = fsPop state
                        flSize = M.size fl
                    --If items exist in the free list, recycle in make, otherwise add on to heap.
                    in if (null fl)
                        then
                            let hp' = M.insert hs v hp
                                hs' = hs + 1
                                state'' = fsPush (Box hs) state'
                            in doNode (Expression rest) (EDState{stack = stack state'', fns = fns state'', vars = vars state'',
                                frames = frames state'', heap = Heap{freeList = fl, h = hp', heapSize = hs'}, ops = ops state'', ioOps = ioOps state''})
                        else
                            let ((replaceBn, _), fl') = M.deleteFindMin fl
                                hp' = M.insert replaceBn v hp
                                state'' = fsPush (Box replaceBn) state'
                            in doNode (Expression rest) (EDState{stack = stack state'', fns = fns state'', 
                                vars = vars state'', frames = frames state'', 
                                heap = Heap{freeList = fl', h = hp', heapSize = hs}, ops = ops state'', ioOps = ioOps state''})

        "open" -> 
            if (null $ stack state)
                then 
                    throwError "Operator (box open) error. Can't open a Box with an empty stack! No Box to open!" state
                else
                    case (fsTop state) of
                        Box n -> 
                            case (validateBox (heap state) n) of
                                Left v -> doNode (Expression rest) (fsPush v state)
                                Right err -> throwError err state 
                        x -> 
                            let xType = chrs $ doQueryType' x 
                            in throwError ("Operator (box open) error. Top of stack needs to be of type Box! Attempted type: " ++ xType) state
        "altr" -> 
            case (stack state) of 
                [] -> throwError "Operator (box altr) error. Two operands expected on stack; none provided!" state
                [x] -> throwError "Operator (box altr) error. Two operands expected on stack; only one provided!" state
                vals ->
                    let (state', secondToTop, top) = fsPop2 state
                    in case (secondToTop, top) of
                        (Box bn, v) ->
                            case (validateBox (heap state') bn) of
                                Left oldV -> 
                                    if (compareTypesForMut oldV v)
                                        then
                                            let h' = M.insert bn v (h $ heap state')
                                                state'' = fsPush (Box bn) state'
                                            in doNode (Expression rest) (EDState{stack = stack state'', fns = fns state'', vars = vars state'', 
                                                frames = frames state'', heap = Heap{freeList = freeList $ heap state'', h = h', 
                                                heapSize = heapSize $ heap state}, ops = ops state'', ioOps = ioOps state''})
                                        else
                                            let (oldVType, vType) = findTypeStrsForError oldV v
                                            in throwError ("Operator (box altr) error. New value for Box " ++ (show bn) ++ 
                                                " of type " ++ vType ++ " doesn't match old value of type " ++ oldVType 
                                                ++ ". Types must match for value to be changed for given Box!") state'
                                Right err -> throwError err state
                        (x, v) ->
                            let (xType, vType) = findTypeStrsForError x v
                            in throwError ("Operator (box altr) error. Second to top of stack needs to be type Box and top needs to be type Value. TL;DR Needs types Box Value ; Attempted types: "
                                ++ xType ++ " and " ++ vType) state'
        "free" -> 
            if (null $ stack state)
                then
                    throwError "Operator (box free) error. Can't free a Box when stack is empty and no Box exists!" state
                else
                    let (state', top) = fsPop state
                    in case (top) of
                        Box freeBn ->
                            case (validateBox (heap state) freeBn) of
                                Left _ ->
                                    let fl' = M.insert freeBn () (freeList $ heap state')
                                    in doNode (Expression rest) (EDState{stack = stack state', fns = fns state', vars = vars state', frames = frames state', 
                                        heap = Heap{freeList = fl', h = (h $ heap state'), 
                                        heapSize = (heapSize $ heap state')}, ops = ops state', ioOps = ioOps state'})
                                Right err -> throwError err state'
                        x ->
                            let xType = chrs $ doQueryType' x
                            in throwError ("Operator (box free) error. Top of stack needs to be of type Box to be free'd! Attempted type: " 
                                ++ xType) state
        "null" -> doNode (Expression rest) (fsPush (Box (-1)) state)
        x -> throwError ("Operator (box) error. Invalid Box command " ++ x ++ " given! Valid commands: make, open, altr, free, null") state

-- Runs true branch if top of stack is true 
--and false branch if top of stack is false.
doNode (Expression((If { ifTrue = trueBranch, ifFalse = falseBranch }):rest)) state = 
    if (null $ stack state)
        then
            throwError "If statement error. No Boolean for if to check because stack is empty!" state
        else do
            state' <- case (fsTop state) of 
                (Boolean True) -> doNode trueBranch (addFrame state)
                (Boolean False) -> doNode falseBranch (addFrame state)
                x ->
                    let xType = chrs $ doQueryType' x
                    in throwError ("If statement error. If statement requires top of the stack" 
                        ++ " to be type Boolean to branch! Attempted type: " ++ xType) state
            doNode (Expression rest) state'

--Pattern matches function def and call.
doNode (Expression((Function {funcCmd = cmd, funcName = name, funcBod = body}):rest)) state =
    case (astNodeToString cmd) of 
        "def" -> 
            let fName = astNodeToString name
            in case (M.lookup fName (fns state)) of 
                    Just bod -> throwError ("Function Def Error. Function " ++ fName ++ " already exists!") state
                    Nothing ->
                        let fns' = M.insert fName body (fns state)
                        in doNode (Expression rest) (EDState{stack = (stack state), fns = fns', vars = (vars state), 
                            frames = (frames state), heap = heap state, ops = ops state, ioOps = ioOps state})

        "call" -> 
            let fName = astNodeToString name
            in case (M.lookup fName (fns state)) of
                Just bod -> 
                    (doNode bod (addFrame state)) >>= (\state' -> doNode (Expression rest) state')
                
                Nothing -> throwError ("Function Call Error. Function " ++ fName ++ " isn't defined!") state

        other -> throwError ("Function Error. Invalid function command given. Given: " ++ other ++ " Valid: def, call") state

--Runs all the different cases of variable actions.
doNode (Expression((Variable{varName = name, varCmd = cmd}):rest)) state =
    case (astNodeToString cmd) of
        --Used in making a new variable in vars.
        "mak" ->
            let vName = astNodeToString name
            in if (null $ stack state)
                then 
                    throwError ("Variable (var) Mak Error. Can't create variable when stack is empty. Attempted variable name: " ++ vName) state 
                else
                    case (M.lookup vName (vars state)) of
                        Just _ -> throwError ("Variable (var) Mak Error. Variable " ++ vName ++ " already exists.") state
                        Nothing -> 
                            let vars' = M.insert vName (fsTop state) (vars state)
                            in doNode (Expression rest) (EDState{stack = (stack state), fns = (fns state), vars = vars', 
                                frames = (frames state), heap = heap state, ops = ops state, ioOps = ioOps state})
                           
        --Pushes variable value to stack.
        "get" -> 
            let vName = astNodeToString name
            in case (M.lookup vName (vars state)) of
                Just v -> doNode (Expression rest) (fsPush v state)
                Nothing -> throwError ("Variable (var) Get Error. Variable " ++ vName ++ " doesn't exist or was deleted!") state

        --Removes variable from existence since var is manually scoped.
        "del" -> 
            let vName = astNodeToString name
            in case (M.lookup vName (vars state)) of 
                Just v -> 
                    let vars' = M.delete vName (vars state)
                    in doNode (Expression rest) (EDState{stack = (stack state), fns = (fns state), vars = vars', frames = 
                        (frames state), heap = heap state, ops = ops state, ioOps = ioOps state}) 
                Nothing -> throwError ("Variable (var) Del Error. Variable " ++ vName ++ " doesn't exist or was already deleted!") state

        --Alters variable to new value on top of stack if the types match.
        "mut" -> 
            let vName = astNodeToString name
            in if (null $ stack state)
                then
                    throwError ("Variable (var) Mut Error. Can't mutate variable when stack is empty! Attempted variable name: " ++ vName) state
                else
                    let newVal = fsTop state
                    in case (M.lookup vName (vars state)) of 
                        Just v -> 
                            if (compareTypesForMut v newVal)
                                then 
                                    let vars' = M.insert vName newVal (vars state)
                                    in doNode (Expression rest) (EDState{stack = (stack state), fns = (fns state), vars = vars', 
                                        frames = (frames state), heap = heap state, ops = ops state, ioOps = ioOps state})
                                else
                                    throwError ("Variable (var) Mut Error. Can't mutate variable " 
                                        ++ vName ++ " of type " ++ (chrs $ doQueryType' v) 
                                        ++ " to different type: " ++ (chrs $ doQueryType' newVal)) state

                        Nothing -> throwError ("Variable (var) Mut Error. Variable " ++ vName ++ " doesn't exist or was deleted!") state

        other -> throwError ("Variable Command Error. Invalid variable command given! Given: " ++ other ++ " Valid: mak, get, mut, del") state

--Runs all the different cases of local variable actions.
doNode LocVar{name = name, cmd = cmd} state =
    case (astNodeToString cmd) of
        "mak" ->
            let vName = astNodeToString name
            in if (null $ stack state)
                then throwError ("Local Variable (loc) Mak Error. Can't create local variable when stack is empty! Attempted local variable name: " ++ vName) state 
                else
                    case (M.lookup vName (head $ frames state)) of
                        Just _ -> throwError ("Local Variable (loc) Mak Error. Local variable " ++ vName ++ " already exists in current scope.") state
                        Nothing ->
                            let frame' = M.insert vName (fsTop state) (head $ frames state)
                            in return EDState{stack = (stack state), fns = (fns state), vars = (vars state), frames = (frame' : (tail $ frames state)), heap = heap state, ops = ops state, ioOps = ioOps state}
                           
        "get" ->  
            case (getLoc (frames state) (astNodeToString name)) of
                Just value -> return (fsPush value state)
                Nothing -> throwError ("Local Variable (loc) Get Error. Local Variable " ++ (astNodeToString name) ++ " not defined in any scope!") state

        "mut" -> 
            let vName = astNodeToString name
            in if (null $ stack state)
                then throwError ("Local Variable (loc) Mut Error. Can't mutate local variable when stack is empty! Attempted local variable name: " ++ vName) state
                else
                    case (getLoc (frames state) vName) of
                        Just v -> 
                            if (compareTypesForMut (fsTop state) v)
                                then
                                    return EDState{stack = (stack state), fns = (fns state), vars = (vars state), 
                                        frames = (updateFrames (frames state) [] (fsTop state) vName False), heap = heap state, ops = ops state, ioOps = ioOps state}
                                else
                                    throwError ("Local Variable (loc) Mut Error. Can't mutate local variable " 
                                        ++ vName ++ " of type " ++ (chrs $ doQueryType' v) 
                                        ++ " to different type: " ++ (chrs $ doQueryType' (fsTop state))) state

                        Nothing -> throwError ("Local Variable (loc) Mut Error. Local Variable " ++ vName ++ " not defined for mutation in any scope!") state

        other -> throwError ("Local Variable (loc) Command Error. Invalid local variable command given! Given: " ++ other ++ " valid: mak, get, mut") state

--This code looks sus but basically it runs the loop body 
-- and then runs the whole loop recursion if it needs to run again.                                                                                                                      
doNode ( While loopBody ) state = 
    if (null $ stack state)
        then throwError "While Loop Error. No Boolean value for while loop to check because stack is empty!" state
        else do 
            newState <- case (fsTop state) of 
                Boolean True -> doNode loopBody (addFrame state)
                Boolean False -> return state
                x -> 
                    let xType = chrs $ doQueryType' x 
                    in throwError ("While Loop Error. Top of stack needs to be type Boolean for loop to try and run! Attempted type: " 
                        ++ xType) state

            --Checks to see if loop can run again or not.
            if (null $ stack newState)
                then throwError "While Loop Error. No Boolean value for while loop to check because stack is empty!" state
                else 
                    case (fsTop newState) of 
                        Boolean True -> doNode (While loopBody) newState
                        Boolean False -> return newState
                        x -> 
                            let xType = chrs $ doQueryType' x 
                            in throwError ("While Loop Error. Top of stack needs to be type Boolean for loop to try and run! Attempted type: " 
                                ++ xType) newState

-- doing a terminal changes depending on whether it's a word or a number. 
-- if it's a number, push it...
doNode ( Terminal ( Val v ) ) state = return $ fsPush v state

--Operation executes either functionally or monadically.
doNode (Expression( (Terminal(Word o)):rest )) state = 
    case HM.lookup o (ops state) of
        Just func -> case (func state) of 
                Left state' -> doNode (Expression rest) state'
                Right err -> throwError err state
        Nothing -> case HM.lookup o (ioOps state) of
            Just ioFunc -> (ioFunc state) >>= (\state' -> doNode (Expression rest) state')
            Nothing -> throwError ("Unrecognized operator: " ++ o) state 

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
    else error ( "Ended expression without finding one of: " ++ intercalate ", " terminators )

-- if tokens remain, keep parsing
parseExpression' alreadyParsed ( token:tokens ) terminators 
    -- found a terminator: stop parsing and return. 
    | token `elem` map Word terminators = ( alreadyParsed, tokens, Just token )

    -- found an if-statement: remove the "if" token, parse the true and false branches, and 
    -- then parse whatever is after the if-statement.
    | token == Word "if" = 
        let ( trueBranch, falseBranch, remTokens ) = parseIf tokens
            newParsed = If{ifTrue = trueBranch, ifFalse = falseBranch} : alreadyParsed
        in parseExpression' newParsed remTokens terminators
            
    -- found a while-statement: remove the "while", parse the body, then parse whatever is after
    | token == Word "while" = 
        let (bod, remTokens) = parseWhile tokens
            newParsed = (While bod) : alreadyParsed
        in parseExpression' newParsed remTokens terminators                                              
    
    --Parse function definition.
    | token == Word "func" = 
        let (cmd, name, bod, remTokens) = parseFuncOp tokens
            newParsed = Function{funcCmd = cmd, funcName = name, funcBod = bod} : alreadyParsed
        in parseExpression' newParsed remTokens terminators

    --Parse basic variable case.
    | token == Word "var" = 
        let (varAct, variableName, remTokens) = parseVarAction tokens
            newParsed = Variable{varName = variableName, varCmd = varAct} : alreadyParsed
        in parseExpression' newParsed remTokens terminators

    --Parse local variable.
    | token == Word "loc" =
        let (varAct, variableName, remTokens) = parseVarAction tokens
            newParsed = LocVar{name = variableName, cmd = varAct} : alreadyParsed
        in parseExpression' newParsed remTokens terminators

    --Parse attErr block.
    | token == Word "attempt" =
        let (attemptBranch, errorBranch, remTokens) = parseAttErr tokens
        in parseExpression' (AttErr{attempt = attemptBranch, onError = errorBranch} : alreadyParsed) remTokens terminators
        
    --Parse tempStackChange block
    | token == Word "tempStackChange" = 
        let (runBlock, remTokens) = parseTempStackChange tokens
        in  parseExpression' ((TempStackChange runBlock) : alreadyParsed) remTokens terminators

    | token == Word "box" = 
        let (boxWords, remTokens, term) = parseExpression' [] tokens [";"]
        in parseExpression' ((BoxOp $ head $ reverse boxWords) : alreadyParsed) remTokens terminators

    -- no special word found. We are parsing a list of operations. Keep doing this until 
    -- there aren't any. 
    | otherwise = parseExpression' ((Terminal token) : alreadyParsed) (tokens) (terminators)

-- takes the result of parseExpression' and wraps it in an Expression constructor
parseExpression :: [Token] -> AstNode
parseExpression tokens = 
    let (nodes, toks, potTok) = parseExpression' [] tokens []
    in Expression(reverse nodes)

--Custom trace function used during debugging. Made by Grant.
traceThing :: (Show a) => a -> a 
traceThing x = traceShow x x 

parseTempStackChange :: [Token] -> (AstNode, [Token])
parseTempStackChange [] = error "tempStackChange missing closing semicolon!"
parseTempStackChange tokens =
    let (runBlock, remTokens, terminator) = parseExpression' [] tokens [";"]
    in (Expression $ reverse runBlock, remTokens)

--Parses an attErr code block into its appropriate expression.
parseAttErr :: [Token] -> (AstNode, AstNode, [Token])
parseAttErr tokens = 
    let (attBranch, remainingTokens, terminator ) = parseExpression' [] tokens [ "onError", ";" ]
        (errBranch, remTokens) = if terminator == (Just $ Word "onError") 
            then parseErrorBranch remainingTokens 
            else error "attempt onError Error:\n Branch onError branch missing.\nUSAGE: attempt CODE_TO_ATTEMPT onError CODE_TO_HANDLE_ERROR ;"
    in (Expression $ reverse attBranch , errBranch, remTokens)

--Parses the onError branch of attErr.
parseErrorBranch :: [Token] -> (AstNode, [Token])
parseErrorBranch tokens = 
    let (errorHandleCode, remTokens, terminator) = parseExpression' [] tokens [";"]
    in (Expression $ reverse errorHandleCode, remTokens)

parseVarAction :: [Token] -> (AstNode, AstNode, [Token])
parseVarAction (token:tokens) = 
    let (varInfo, remTokens, terminator) = parseExpression' [] tokens [";"]
        revVarInfo = reverse varInfo
        varAction = Terminal token
        varName = if not $ null (tail revVarInfo) 
            then error "Malformed variable command" else head revVarInfo
        in (varAction, varName, remTokens)

--Parses a function definition.
parseFuncOp :: [Token] -> (AstNode, AstNode, AstNode, [Token])
parseFuncOp (command:name:tokens) =
    let (funcBody, remTokens, terminator) = parseExpression' [] tokens [";"]
        funcCommand = Terminal command
        funcName = Terminal name
        in (funcCommand, funcName, Expression $ reverse funcBody, remTokens)

-- we just saw an "if". now we have to build an "If" AstNode.
-- returns the two branches and the remaining tokens. 
-- ( ifTrue, ifFalse, remainingTokens ). 
parseIf :: [Token] -> ( AstNode, AstNode, [Token] ) 
parseIf tokens = 
    let ( trueBranch, remainingTokens, terminator ) = parseExpression' [] tokens [ "else", ";" ]
        (falseBranch, remTokens) = if terminator == (Just $ Word "else") 
            then parseElse remainingTokens 
            else (Expression([]), remainingTokens)
    in (Expression $ reverse trueBranch, falseBranch, remTokens)

-- we just saw an "else". now finish the ifFalse part of the If node. This one only needs to 
-- return the "false" branch of the if statement, which is why there is only one [AstNode] in 
-- the return value. 
parseElse :: [Token] -> (  AstNode, [Token] )                                                               
parseElse tokens = 
    let (ifFalse, remTokens, terminator) = parseExpression' [] tokens [";"]
    in (Expression $ reverse ifFalse, remTokens)

-- parsing a while loop is similar to parsing an if statement. 
parseWhile :: [Token] -> ( AstNode, [Token] )
-- if we reach the end of our tokens without closing the loop, that's an error 
parseWhile [] = error "while without closing semicolon."
-- otherwise, parse the loop body until reaching the ";" 
parseWhile tokens = let (loopBod, remTokens, terminator) = parseExpression' [] tokens [";"]
                    in (Expression $ reverse loopBod, (remTokens))
    
--Makes new interpretor state with default values.
edStateNew :: EDState
edStateNew = let opsTup = createOpsHashes in EDState { stack = [], fns = M.empty, vars = M.empty, frames = [M.empty], heap = Heap{freeList = M.empty, h = M.empty, heapSize = 0}, ops = fst opsTup, ioOps = snd opsTup}

-- push a new value onto the stack
fsPush :: Value -> EDState -> EDState
fsPush val state = EDState { stack = (val : (stack state)), fns = (fns state), vars = (vars state), frames = (frames state), heap = heap state, ops = ops state, ioOps = ioOps state}

--Removes value from top of stack, returning it.
fsPop :: EDState -> ( EDState, Value )
fsPop state = 
    let top = head (stack state) 
        newStack = tail (stack state)  
    in  ( EDState { stack = newStack, fns = (fns state), vars = (vars state), frames = (frames state), heap = heap state, ops = ops state, ioOps = ioOps state}, top )

--Removes the top two elements from the stack, returning them.
fsPop2 :: EDState -> ( EDState, Value, Value )
fsPop2 state = 
    let (state', top) = fsPop state
        (state'', secondToTop) = fsPop state'
    in  (EDState {stack = (stack state''), fns = (fns state), vars = (vars state), frames = (frames state), heap = heap state, ops = ops state, ioOps = ioOps state}, secondToTop, top)

--Removes top three elements from stack.
fsPop3 :: EDState -> ( EDState, Value, Value, Value )
fsPop3 state = 
    let (state', top) = fsPop state
        (state'', secondToTop) = fsPop state'
        (state''', thirdToTop) = fsPop state''
    in (EDState {stack = (stack state'''), fns = (fns state), vars = (vars state), frames = (frames state), heap = heap state, ops = ops state, ioOps = ioOps state}, thirdToTop, secondToTop, top)

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
--Makes it so it prints Box NULL instead of Box -1 since -1 is defined as the null value under the hood.
printStack ((Box (-1)):xs) = putStrLn "Box NULL" >> printStack xs 
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
                Box (-1) -> acc ++ (if (index > 0) then ", " else "") ++ "Box NULL"
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
            Box (-1) -> "Box NULL"
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
        let tokens = removeComments False [] ( tokenize code )

        --Parse and run AST, printing result.
        let ast = parseExpression tokens
        finalState <- (doNode ast edStateNew)
        
        --putStrLn $ show $ length $ frames finalState

        printStack $ reverse $ (stack finalState) 