--Jesse A. Jones
--28 Apr, 2023
--Toy Programming Language Named EcksDee

{-
    ISSUES:
        -String doesn't work with spaces in it.
-}

import Data.List
import Data.Char
import Data.Maybe 
import Debug.Trace
import qualified Data.Map.Strict as M

data Value =                                         
        BigInteger Integer
    |   Integer Int
    |   Float Float
    |   String String
    |   Char Char
    |   Boolean Bool
    |   Array [Value]
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
data ForthState = ForthState { 
    stack :: [Value], 
    names :: M.Map String AstNode
} deriving ( Show )

--Adds two values together. If the types can't be added, throw an error.
--Can also act as an OR operator for Boolean type.
addVals :: Value -> Value -> Value
addVals (BigInteger a) (BigInteger b) = BigInteger (a + b)
addVals (Integer a) (Integer b) = Integer (a + b)
addVals (Float a) (Float b) = Float (a + b)
addVals (Boolean a) (Boolean b) = Boolean (a || b)
addVals _ _ = error "Operator (+) error. \n Can't add types together that aren't BigIntegers, Integers, Floats, or Booleans. \n Data types also need to match."

-- Subtracts two values. If the types can't be subtracted, throw an error.
subVals :: Value -> Value -> Value
subVals (BigInteger a) (BigInteger b) = BigInteger (b - a)
subVals (Integer a) (Integer b) = Integer (b - a)
subVals (Float a) (Float b) = Float (b - a)
subVals _ _ = error "Operator (-) error. \n Can't subtract types that aren't BigIntegers, Integers, or Floats. \n Data types also need to match."

--Multiplies two values together. If the types can't be multiplied, throw an error.
multVals :: Value -> Value -> Value
multVals (BigInteger a) (BigInteger b) = BigInteger (a * b)
multVals (Integer a) (Integer b) = Integer (a * b)
multVals (Float a) (Float b) = Float (a * b)
multVals (Boolean a) (Boolean b) = Boolean (a && b)
multVals _ _ = error "Operator (*) error. \n Can't multiply types together that aren't BigIntegers, Integers, Floats, or Booleans. \n Data types also need to match."

-- Divides two values. If the types can't be divided, throw an error.
divideVals :: Value -> Value -> Value
divideVals (BigInteger a) (BigInteger b) = BigInteger (b `div` a)
divideVals (Integer a) (Integer b) = Integer (b `div` a)
divideVals (Float a) (Float b) = Float (b / a)
divideVals _ _ = error "Operator (/) error. \n Can't divide types that aren't BigIntegers, Integers, or Floats. \n Data types also need to match"

modVals :: Value -> Value -> Value
modVals (BigInteger a) (BigInteger b) = BigInteger (b `mod` a)
modVals (Integer a) (Integer b) = Integer (b `mod` a)
modVals _ _ = error "Operator (%) error. \n Can't perform modulo on types that aren't BigIntegers or Integers. \n Data types also have to match."

doAdd :: ForthState -> ForthState
doAdd ForthState{stack = [], names = ns} = 
    error "Operator (+) error. Addition requires two operands!"
doAdd ForthState{stack = [x], names = ns} = 
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
doSub :: ForthState -> ForthState
doSub ForthState{stack = [], names = ns} = 
    error "Operator (-) error. Subtraction requires two operands!"
doSub ForthState{stack = [x], names = ns} = 
    error "Operator (-) error. Subtraction requires two operands!"
doSub state =
    let (stateNew, b, a) = fsPop2 state
    in fsPush (subVals a b) stateNew

-- apply the * operation: pop 2 values, multiply them, push the result. 3 4 * -> 12
doMul :: ForthState -> ForthState
doMul ForthState{stack = [], names = ns} = 
    error "Operator (*) error. Multiplication requires two operands!"
doMul ForthState{stack = [x], names = ns} = 
    error "Operator (*) error. Multiplication requires two operands!"
doMul state = 
    let (stateNew, b, a) = fsPop2 state
    in fsPush (multVals a b) stateNew

-- apply the / operation: pop 2 values, divide them, push the result. 4 2 / -> 2
doDiv :: ForthState -> ForthState
doDiv ForthState{stack = [], names = ns} = 
    error "Operator (/) error. Division requires two operands!"
doDiv ForthState{stack = [x], names = ns} = 
    error "Operator (/) error. Division requires two operands!"
doDiv state = 
    let (stateNew, b, a) = fsPop2 state
    in fsPush (divideVals a b) stateNew

--Apply modulo operation
doModulo :: ForthState -> ForthState
doModulo ForthState{stack = [], names = ns} = 
    error "Operator (%) error. Modulo requires two operands!"
doModulo ForthState{stack = [x], names = ns} = 
    error "Operator (%) error. Modulo requires two operands!"
doModulo state =
    let (state', b, a) = fsPop2 state
    in fsPush (modVals a b) state'

-- apply the swap operation. pop 2 values, re-push them in reverse order. 1 2 swap -> 2 1 
doSwap :: ForthState -> ForthState 
doSwap ForthState{stack = [], names = ns} = ForthState{stack = [], names = ns}
doSwap ForthState{stack = [x], names = ns} = ForthState{stack = [x], names = ns}
doSwap state = 
    let (stateNew, b, a) = fsPop2 state
        stateNewer = fsPush a stateNew
    in fsPush b stateNewer

-- apply the drop operation. pop 1 value. 1 2 3 -> 1 2 
-- does nothing if stack is empty 
doDrop :: ForthState -> ForthState
doDrop state = 
    if null $ stack state then state
    else 
        let (stateNew, a) = fsPop state
        in stateNew    

-- apply the rot operation. rotates the top three right: 1 2 3 -> 3 1 2 
-- does nothing if stack is empty or size 1
-- same as swap if stack has size 2 
doRot :: ForthState -> ForthState
--Pattern matches small stack cases.
doRot ForthState{stack = [], names = ns} = ForthState {stack = [], names = ns}
doRot ForthState{stack = [x], names = ns} = ForthState {stack = [x], names = ns}
doRot ForthState{stack = [x, y], names = ns} = ForthState {stack = [y, x], names = ns}

--Peforms rotation if stack is 3 or greater in size.
doRot state =
    let (popState, c, b, a) = fsPop3 state
        statePush1 = fsPush a popState
        statePush2 = fsPush c statePush1
    in fsPush b statePush2   

-- duplicate the top value on the stack. 1 -> 1 1 
doDup :: ForthState -> ForthState

--Nothing to duplicate if stack is empty.
doDup ForthState{stack = [], names = ns} = ForthState {stack = [], names = ns}

--Duplicates top of stack.
doDup state = 
    let (newState, top) = fsPop state
        push1 = fsPush top newState
    in fsPush top push1 

--Tests equality. Pushes 1 if equal, 0 if not equal.
doEqual :: ForthState -> ForthState
doEqual ForthState{stack = [], names = ns} = 
    error "Eqality comparison requires two operands!"
doEqual ForthState{stack = [x], names = ns} = 
    error "Eqality comparison requires two operands!"
doEqual state = 
    let (state', secondToTop, top) = fsPop2 state
    in if (top == secondToTop) 
            then fsPush (Boolean True) state' 
            else fsPush (Boolean False) state'

--Tests inequality. Pushes 1 if not equal, 0 if equal.

doNotEqual :: ForthState -> ForthState
doNotEqual ForthState{stack = [], names = ns} = 
    error "Ineqality comparison requires two operands!"
doNotEqual ForthState{stack = [x], names = ns} = 
    error "Ineqality comparison requires two operands!"
doNotEqual state = 
    let (state', secondToTop, top) = fsPop2 state
    in if (top /= secondToTop) 
            then fsPush (Boolean True) state' 
            else fsPush (Boolean False) state'

--Tests if second to top of stack is greater than top of stack. 
-- Pushes 1 if true, 0 if false.
doGreaterThan :: ForthState -> ForthState
doGreaterThan ForthState{stack = [], names = ns} = 
    error "Greater than comparison requires two operands!"
doGreaterThan ForthState{stack = [x], names = ns} = 
    error "Greater than comparison requires two operands!"
doGreaterThan state = 
    let (state', secondToTop, top) = fsPop2 state
    in if (secondToTop > top) 
            then fsPush (Boolean True) state' 
            else fsPush (Boolean False) state'

--Tests if second to top of stack is less than top of stack. 
-- Pushes 1 if true, 0 if false.
doLessThan :: ForthState -> ForthState
doLessThan ForthState{stack = [], names = ns} = 
    error "Less than comparison requires two operands!"
doLessThan ForthState{stack = [x], names = ns} = 
    error "Less than comparison requires two operands!"
doLessThan state = 
    let (state', secondToTop, top) = fsPop2 state
    in if (secondToTop < top) 
            then fsPush (Boolean True) state' 
            else fsPush (Boolean False) state'

--Tests if second to top of stack is greater than equal to top of stack. 
-- Pushes 1 if true, 0 if false.
doGreaterThanEqualTo :: ForthState -> ForthState
doGreaterThanEqualTo ForthState{stack = [], names = ns} = 
    error "Greater than equal to comparison requires two operands!"
doGreaterThanEqualTo ForthState{stack = [x], names = ns} = 
    error "Greater than equal to comparison requires two operands!"
doGreaterThanEqualTo state = 
    let (state', secondToTop, top) = fsPop2 state
    in if (secondToTop >= top) 
            then fsPush (Boolean True) state' 
            else fsPush (Boolean False) state'

--Tests if second to top of stack is less than equal to top of stack. 
-- Pushes 1 if true, 0 if false.
doLessThanEqualTo :: ForthState -> ForthState
doLessThanEqualTo ForthState{stack = [], names = ns} = 
    error "Less than equal to comparison requires two operands!"
doLessThanEqualTo ForthState{stack = [x], names = ns} = 
    error "Less than equal to comparison requires two operands!"
doLessThanEqualTo state = 
    let (state', secondToTop, top) = fsPop2 state
    in if (secondToTop <= top) 
            then fsPush (Boolean True) state' 
            else fsPush (Boolean False) state'


-- performs the operation identified by the string. for example, doOp state "+"
-- will perform the "+" operation, meaning that it will pop two values, sum them,
-- and push the result. 
doOp :: String -> ForthState -> ForthState
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

-- Error thrown if reached here.
doOp op = error $ "unrecognized word: " ++ op 

astNodeToString :: AstNode -> String
astNodeToString (Terminal (Word w)) = w

astNodeToValue :: AstNode -> Value
astNodeToValue (Terminal (Val v)) = v

makeVar :: ForthState -> String -> ForthState
makeVar state varName = 
    let lkup = M.lookup varName (names state)
    --Throw error if variable exists. Otherwise, make variable by inserting it into hash table.
    in case lkup of
        Just _ -> error "Variable Mak Error: Variable already exists."
        Nothing -> let names' = M.insert varName (Terminal $ Val $ fsTop state) (names state)
            in ForthState{stack = (stack state), names = names'}

--Comapres types in order to enforce static typing when mutating variables.
compareTypesForMut :: Value -> Value -> Bool
compareTypesForMut (Boolean _) (Boolean _) = True
compareTypesForMut (BigInteger _) (BigInteger _) = True
compareTypesForMut (Integer _) (Integer _) = True
compareTypesForMut (Float _) (Float _) = True
compareTypesForMut (String _) (String _) = True
compareTypesForMut (Char _) (Char _) = True
compareTypesForMut _ _ = False

mutateVar :: ForthState -> String -> ForthState
mutateVar state varName =
    let lkupVal = M.lookup varName (names state)
        newVal = fsTop state
    --If variable exists it can be mutated. Otherwise, an error is thrown.
    in case lkupVal of
        Just value -> if compareTypesForMut (astNodeToValue value) newVal then 
            let names' = M.insert varName (Terminal $ Val $ fsTop state) (names state)
            in ForthState{stack = (stack state), names = names'}
            else error "Variable Mut Error: Can't mutate variable to different type."
        Nothing -> error "Variable Mut Error: Variable doesn't exist or was deleted"

funcDef :: ForthState -> String -> AstNode -> ForthState
funcDef state funcName funcBod = 
    let look = M.lookup funcName (names state)
    in case look of 
        Just bod -> error "Function Def Error: Function of same name already exists" 
        Nothing -> let names' = M.insert funcName funcBod (names state)
                   in ForthState{stack = (stack state), names = names'}

funcCall :: ForthState -> String -> (ForthState, AstNode)
funcCall state funcName = 
    let look = M.lookup funcName (names state)
    in case look of 
        Just body -> (state, body)
        Nothing -> error "Function Call Error: Function isn't defined."

-- execute an AstNode
doNode :: AstNode -> ForthState -> ForthState

-- if we execute a terminal that's an if-statement, we need to determine whether
-- the top of the stack is "true" (/= 0.0)
doNode If { ifTrue = trueBranch, ifFalse = falseBranch } state = 
    let top = fsTop state
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
                           
        "get" -> let lkup = M.lookup (astNodeToString name) (names state) 
                 in case lkup of
                    Just value -> let stack' = fsPush (astNodeToValue value) state
                              in doNode (Expression rest) stack' 
                    Nothing -> error "Variable Get Error: Variable doesn't exist or was deleted"

        "del" -> let lkup = M.lookup (astNodeToString name) (names state) 
                 in case lkup of
                    Just value -> let names' = M.delete (astNodeToString name) (names state)
                           in doNode (Expression rest) (ForthState{stack = (stack state), names = names'})
                    Nothing -> error "Variable Del Error: Variable doesn't exist" 

        "mut" -> if null (stack state) 
            then error "Variable Mut Error: Can't mutate variable when stack is empty."
            else
                doNode (Expression rest) (mutateVar state (astNodeToString name))

        _ -> error "Variable Command Error: Invalid variable command given. Valid: mak, get, mut, del"

--Runs while loop.
doNode ( While loopBody ) state =
    let top = fsTop state

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
fsNew :: ForthState
fsNew = ForthState { stack = [], names = M.empty }

-- push a new value onto the stack
fsPush :: Value -> ForthState -> ForthState
fsPush val state = ForthState { stack = val : stack state, names = (names state)}

-- remove a value from the stack, or print an error if nothing is there.
-- returns the value removed and the new state 
fsPop :: ForthState -> ( ForthState, Value )
fsPop state = 
    let top = head $ stack state 
        new_stack = tail $ stack state  
    in  
        ( ForthState { stack = new_stack, names = (names state) }, top )

-- remove two values from the stack. return the new stack and the two items.
fsPop2 :: ForthState -> ( ForthState, Value, Value )
fsPop2 state = 
    let top  = head $ stack state
        secondToTop = head (tail $ stack state)
        stackNew = tail (tail (stack state))
    in
        (ForthState {stack = stackNew, names = names state}, secondToTop, top)

-- remove three values from the stack. return the new stack and the three items. 
fsPop3 :: ForthState -> ( ForthState, Value, Value, Value )
fsPop3 state = 
    let top = fsTop state
        secondToTop = fsTop ForthState{ stack = (tail $ stack state), 
            names = (names state)}
        thirdToTop = fsTop ForthState{ stack = (tail $ tail $ stack state), 
            names = (names state)}
        stackNew = tail $ tail $ tail $ stack state
    in
        (ForthState {stack = stackNew, names = (names state)}, 
            thirdToTop, secondToTop, top) 

-- return the value on top of the stack 
fsTop :: ForthState -> Value 
fsTop state = head $ stack state 

decCount :: String -> Int
decCount "" = 0
decCount (x:xs) = if x == '.' then 1 + decCount xs else decCount xs

isNum' :: String -> Bool -> Bool 
isNum' "" isNum  = isNum  
isNum' (x:xs) isNum =
    let nums = "0123456789"
    in if not (x `elem` nums || x == '.') 
        then isNum' xs False
        else isNum' xs isNum

--Determines if a string is a valid number.
isNum :: String -> Int
isNum "" = -1
isNum numStr = 
    let containsValidChars = isNum' numStr True
        decimalPoints = decCount numStr
    in if containsValidChars 
        then case decimalPoints of
            0 -> 0
            1 -> 1
            _ -> -1
        else -1 

-- Used to turn the strings into values and other tokens.
lexToken :: String -> Token
lexToken t
    | t == "true" = Val $ Boolean True
    | t == "false" = Val $ Boolean False
    | (head t) == '"' && (last t) == '"' = Val $ String (read t :: String)  --String case                                                                  
    | (head t) == '\'' && (last t) == '\'' && length t == 3 = Val $ Char (read t :: Char) --Char case
    | (last t == 'b') && ((isNum (if head t == '-' then tail $ init t else init t)) == 0) = Val $ BigInteger (read (init t) :: Integer) --BigInt case
    | (isNum (if head t == '-' then tail t else t)) == 0 = Val $ Integer (read t :: Int) --Int Case
    | (isNum (if head t == '-' then tail t else t)) == 1 = Val $ Float (read t :: Float) --Float case.
    | otherwise = Word t                             

-- Takes a whole program and turns it into a list of tokens. Calls "lexToken"
tokenize :: String -> [Token]
tokenize code = map lexToken $ words code 

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

main :: IO ()
main = do
    -- get all the code passed to STDIN as a giant string 
    code <- getContents

    -- convert it into a list of tokens
    let tokens = removeComments False [] ( tokenize code )

    -- parse the ast 
    let ast = parseExpression tokens

    --print ast

    print $ reverse $ stack $ doNode ast fsNew 
