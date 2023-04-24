--Jesse A. Jones
--24 Apr, 2023
--Toy Programming Language Named EcksDee

import Data.List
import Data.Char
import Data.Maybe 
import Debug.Trace
import qualified Data.Map.Strict as M

-- data Value =                                         --UNCOMMENT LATER
--         BigInteger Integer
--     |   Integer Int
--     |   Float Float
--     |   String String
--     |   Char Char
--     |   Array [Value]
--     deriving (Eq, Show)

-- or it can be an operation, which has a string name.
-- the program "2 2 +" would have tokens [ I 2, I 2, Op "+" ] 
data Token = 
        Val Float 
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

    |   Function {funcName :: AstNode, funcBod :: AstNode}

    |   Variable {varName :: AstNode, varCmd :: AstNode}

    deriving ( Show )

-- This is the state of the interpreter. 
-- Currently it stores the stack, which is where all of the data lives. 
data ForthState = ForthState { 
    stack :: [Float], 
    names :: M.Map String AstNode
} deriving ( Show )

doAdd :: ForthState -> ForthState
doAdd state = 
    let ( state', b, a  ) = fsPop2 state 
    in fsPush ( a + b ) state' 
-- we need to pop 2 values so we can add them.
-- we will pop 2 values in all the below operations. 
-- you can streamline this by defining a helper function "binary_op" if you want.
-- it can take a function with type Int -> Int -> Int and apply it to the top 
-- two values on the stack, pushing the result. 

-- apply the - operation: pop 2 values, subtract them, push the result. 1 2 - -> -1
doSub :: ForthState -> ForthState
doSub state =
    let (stateNew, b, a) = fsPop2 state
    in fsPush (b - a) stateNew

-- apply the * operation: pop 2 values, multiply them, push the result. 3 4 * -> 12
doMul :: ForthState -> ForthState
doMul state = 
    let (stateNew, b, a) = fsPop2 state
    in fsPush (a * b) stateNew

-- apply the / operation: pop 2 values, divide them, push the result. 4 2 / -> 2
doDiv :: ForthState -> ForthState
doDiv state = 
    let (stateNew, b, a) = fsPop2 state
    in fsPush (b / a) stateNew

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
            then fsPush 1 state' 
            else fsPush 0 state'

--Tests inequality. Pushes 1 if not equal, 0 if equal.

doNotEqual :: ForthState -> ForthState
doNotEqual ForthState{stack = [], names = ns} = 
    error "Ineqality comparison requires two operands!"
doNotEqual ForthState{stack = [x], names = ns} = 
    error "Ineqality comparison requires two operands!"
doNotEqual state = 
    let (state', secondToTop, top) = fsPop2 state
    in if (top /= secondToTop) 
            then fsPush 1 state' 
            else fsPush 0 state'

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
            then fsPush 1 state' 
            else fsPush 0 state'

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
            then fsPush 1 state' 
            else fsPush 0 state'

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
            then fsPush 1 state' 
            else fsPush 0 state'

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
            then fsPush 1 state' 
            else fsPush 0 state'

-- performs the operation identified by the string. for example, doOp state "+"
-- will perform the "+" operation, meaning that it will pop two values, sum them,
-- and push the result. 
doOp :: String -> ForthState -> ForthState
-- here's how we turn the strings into their corresponding operation. 
doOp "+" state = doAdd state
doOp "-" state = doSub state
doOp "*" state = doMul state
doOp "/" state = doDiv state 
doOp "swap" state = doSwap state 
doOp "drop" state = doDrop state 
doOp "rot" state = doRot state 
doOp "dup" state = doDup state
doOp "==" state = doEqual state
doOp "/=" state = doNotEqual state
doOp ">" state = doGreaterThan state
doOp "<" state = doLessThan state
doOp ">=" state = doGreaterThanEqualTo state
doOp "<=" state = doLessThanEqualTo state

-- if we go through all the definitions without finding our operation, 
-- it's not supported unless it's a function call. 
doOp op state = 
    let funcBod = M.lookup op (names state)
    in case funcBod of
        Just funcBod -> doNode funcBod state
        Nothing -> error $ "unrecognized word: " ++ op 

astNodeToString :: AstNode -> String
astNodeToString (Terminal (Word w)) = w

astNodeToFloat :: AstNode -> Float
astNodeToFloat (Terminal (Val v)) = v

makeVar :: ForthState -> String -> ForthState
makeVar state varName = 
    let lkup = M.lookup varName (names state)
    --Throw error if variable exists. Otherwise, make variable by inserting it into hash table.
    in case lkup of
        Just _ -> error "Variable Mak Error: Variable already exists."
        Nothing -> let names' = M.insert varName (Terminal $ Val $ fsTop state) (names state)
            in ForthState{stack = (stack state), names = names'}

mutateVar :: ForthState -> String -> ForthState
mutateVar state varName =
    let lkup = M.lookup varName (names state)
    --If variable exists it can be mutated. Otherwise, an error is thrown.
    in case lkup of
        Just value -> let names' = M.insert varName (Terminal $ Val $ fsTop state) (names state)
            in ForthState{stack = (stack state), names = names'}
        Nothing -> error "Variable Mut Error: Variable doesn't exist or was deleted"

-- execute an AstNode
doNode :: AstNode -> ForthState -> ForthState

-- if we execute a terminal that's an if-statement, we need to determine whether
-- the top of the stack is "true" (/= 0.0)
doNode If { ifTrue = trueBranch, ifFalse = falseBranch } state = 
    let top = fsTop state
    in 
    if (top /= 0.0) then 
        doNode trueBranch state 
    else doNode falseBranch state

--Patterm matches function definition.
doNode (Expression((Function {funcName = name, funcBod = body}):rest)) state =
    let names' = M.insert (astNodeToString name) body (names state)
    in doNode (Expression(rest)) (ForthState{stack = (stack state), names = (names')})

--Runs all the different cases of variable actions.
doNode (Expression((Variable{varName = name, varCmd = cmd}):rest)) state =
    case (astNodeToString cmd) of
        "mak" -> if null (stack state) 
            then error "Variable Mak Error: Can't create variable when stack is empty."
            else
                doNode (Expression rest) (makeVar state (astNodeToString name))
                           

        "get" -> let lkup = M.lookup (astNodeToString name) (names state) 
                 in case lkup of
                    Just value -> let stack' = fsPush (astNodeToFloat value) state
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
        newState = if top /= 0 then
            doNode (loopBody) state
        else state

    --If loop ran and can run again, it's run again, 
    -- otherwise, state is returned.
        newTop = fsTop newState
    in if (newTop /= 0) then doNode ( While loopBody ) newState else newState

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
        let (name, bod, remTokens) = parseFuncDef tokens
            newParsed = alreadyParsed ++ [Function{funcName = name, funcBod = bod}]
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
parseFuncDef :: [Token] -> (AstNode, AstNode, [Token])
parseFuncDef (token:tokens) =
    let (funcBody, remTokens, terminator) = parseExpression' [] tokens [";"]
        funcName = Terminal token
        in (funcName, Expression(funcBody), remTokens)

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
fsPush :: Float -> ForthState -> ForthState
fsPush i state = ForthState { stack = i : stack state, names = (names state)}

-- remove a value from the stack, or print an error if nothing is there.
-- returns the value removed and the new state 
fsPop :: ForthState -> ( ForthState, Float )
fsPop state = 
    let top = head $ stack state 
        new_stack = tail $ stack state  
    in  
        ( ForthState { stack = new_stack, names = (names state) }, top )

-- remove two values from the stack. return the new stack and the two items.
fsPop2 :: ForthState -> ( ForthState, Float, Float )
fsPop2 state = 
    let top  = head $ stack state
        secondToTop = head (tail $ stack state)
        stackNew = tail (tail (stack state))
    in
        (ForthState {stack = stackNew, names = names state}, secondToTop, top)

-- remove three values from the stack. return the new stack and the three items. 
fsPop3 :: ForthState -> ( ForthState, Float, Float, Float )
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
fsTop :: ForthState -> Float 
fsTop state = head $ stack state 

-- Takes a single word and turns it into a token. So "2" becomes "I 2" and 
-- "+" becomes "Op +"
lexToken :: String -> Token
lexToken t = 
    let firstChar = ord . head in 
    if firstChar t >= ord '0' && firstChar t <= ord '9' then 
        Val $ read t 
    else 
        Word t 

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

    print $ reverse $ stack $ doNode ast fsNew 
