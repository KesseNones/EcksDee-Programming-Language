--Jesse A. Jones
--Version: Alpha 0.5.48
--Compiler for EcksDee

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
import System.IO.Error (tryIOError)
import System.Exit (ExitCode(..))
import System.Process (system)
import Data.List (isSuffixOf)

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
    heap :: Heap 
}

data GeneralException = GeneralException String deriving (Show, Typeable)

instance Exception GeneralException

--Used in throwing error.
throwError :: String -> EDState -> IO EDState
throwError msg = throw $ GeneralException msg                                 

--Parses tokens into an abstract syntax tree.
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
fsNew :: IO EDState
fsNew = return EDState { stack = [], fns = M.empty, vars = M.empty, frames = [M.empty], heap = Heap{freeList = M.empty, h = M.empty, heapSize = 0}}

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

nFourSpaces' :: Int -> String -> String
nFourSpaces' 0 acc = acc
nFourSpaces' n acc = 
    nFourSpaces' (n - 1) (' ':' ':' ':' ':acc)

nFourSpaces :: Int -> String
nFourSpaces count = nFourSpaces' count "" 

-- --Runs through the code and executes all nodes of the AST.
-- doNode :: AstNode -> EDState -> IO EDState

-- --Attempt on Error doNode case where it tries to run code in attempt branch 
-- -- and instead runs the code in on Error if there's a problem.
-- doNode AttErr{attempt = att, onError = err} state = catch (doNode att (addFrame state)) handler 
--     where 
--         handler :: GeneralException -> IO EDState
--         handler (GeneralException msg) = 
--             let state' = fsPush (String {chrs = msg, len = length msg}) (addFrame state)
--             in doNode err state'

-- --Pattern matches TempStackChange block. In this block, the code inside it runs but importantly 
-- -- without a stack change like with other operators like this.
-- doNode (TempStackChange runBlock) state =
--     (doNode runBlock (addFrame state)) >>= (\state' -> return EDState{stack = stack state, fns = fns state', vars = vars state', frames = frames state', heap = heap state'})

-- --Parses box command.
-- doNode (BoxOp cmd) state =
--     case (astNodeToString cmd) of
--         "make" -> 
--             if (null $ stack state)
--                 then
--                     throwError "Operator (box make) error. Can't make a box with no data on stack to give it!" state
--                 else
--                     let (Heap{freeList = fl, h = hp, heapSize = hs}) = heap state
--                         (state', v) = fsPop state
--                         flSize = M.size fl
--                     --If items exist in the free list, recycle in make, otherwise add on to heap.
--                     in if (null fl)
--                         then
--                             let hp' = M.insert hs v hp
--                                 hs' = hs + 1
--                                 state'' = fsPush (Box hs) state'
--                             in return EDState{stack = stack state'', fns = fns state'', vars = vars state'',
--                                 frames = frames state'', heap = Heap{freeList = fl, h = hp', heapSize = hs'}}
--                         else
--                             let ((replaceBn, _), fl') = M.deleteFindMin fl
--                                 hp' = M.insert replaceBn v hp
--                                 state'' = fsPush (Box replaceBn) state'
--                             in return EDState{stack = stack state'', fns = fns state'', 
--                                 vars = vars state'', frames = frames state'', 
--                                 heap = Heap{freeList = fl', h = hp', heapSize = hs}}

--         "open" -> 
--             if (null $ stack state)
--                 then 
--                     throwError "Operator (box open) error. Can't open a Box with an empty stack! No Box to open!" state
--                 else
--                     case (fsTop state) of
--                         Box n -> 
--                             case (validateBox (heap state) n) of
--                                 Left v -> return $ fsPush v state
--                                 Right err -> throwError err state 
--                         x -> 
--                             let xType = chrs $ doQueryType' x 
--                             in throwError ("Operator (box open) error. Top of stack needs to be of type Box! Attempted type: " ++ xType) state
--         "altr" -> 
--             case (stack state) of 
--                 [] -> throwError "Operator (box altr) error. Two operands expected on stack; none provided!" state
--                 [x] -> throwError "Operator (box altr) error. Two operands expected on stack; only one provided!" state
--                 vals ->
--                     let (state', secondToTop, top) = fsPop2 state
--                     in case (secondToTop, top) of
--                         (Box bn, v) ->
--                             case (validateBox (heap state') bn) of
--                                 Left oldV -> 
--                                     if (compareTypesForMut oldV v)
--                                         then
--                                             let h' = M.insert bn v (h $ heap state')
--                                                 state'' = fsPush (Box bn) state'
--                                             in return EDState{stack = stack state'', fns = fns state'', vars = vars state'', 
--                                                 frames = frames state'', heap = Heap{freeList = freeList $ heap state'', h = h', heapSize = heapSize $ heap state}}
--                                         else
--                                             let (oldVType, vType) = findTypeStrsForError oldV v
--                                             in throwError ("Operator (box altr) error. New value for Box " ++ (show bn) ++ 
--                                                 " of type " ++ vType ++ " doesn't match old value of type " ++ oldVType 
--                                                 ++ ". Types must match for value to be changed for given Box!") state'
--                                 Right err -> throwError err state
--                         (x, v) ->
--                             let (xType, vType) = findTypeStrsForError x v
--                             in throwError ("Operator (box altr) error. Second to top of stack needs to be type Box and top needs to be type Value. TL;DR Needs types Box Value ; Attempted types: "
--                                 ++ xType ++ " and " ++ vType) state'
--         "free" -> 
--             if (null $ stack state)
--                 then
--                     throwError "Operator (box free) error. Can't free a Box when stack is empty and no Box exists!" state
--                 else
--                     let (state', top) = fsPop state
--                     in case (top) of
--                         Box freeBn ->
--                             case (validateBox (heap state) freeBn) of
--                                 Left _ ->
--                                     let fl' = M.insert freeBn () (freeList $ heap state')
--                                     in return EDState{stack = stack state', fns = fns state', vars = vars state', frames = frames state', 
--                                         heap = Heap{freeList = fl', h = (h $ heap state'), heapSize = (heapSize $ heap state')}}
--                                 Right err -> throwError err state'
--                         x ->
--                             let xType = chrs $ doQueryType' x
--                             in throwError ("Operator (box free) error. Top of stack needs to be of type Box to be free'd! Attempted type: " 
--                                 ++ xType) state
--         "null" -> return $ fsPush (Box (-1)) state
--         x -> throwError ("Operator (box) error. Invalid Box command " ++ x ++ " given! Valid commands: make, open, altr, free, null") state

-- -- Runs true branch if top of stack is true 
-- --and false branch if top of stack is false.
-- doNode If { ifTrue = trueBranch, ifFalse = falseBranch } state = 
--     if (null $ stack state)
--         then
--             throwError "If statement error. No Boolean for if to check because stack is empty!" state
--         else
--             case (fsTop state) of 
--                 (Boolean True) -> doNode trueBranch (addFrame state)
--                 (Boolean False) -> doNode falseBranch (addFrame state)
--                 x ->
--                     let xType = chrs $ doQueryType' x
--                     in throwError ("If statement error. If statement requires top of the stack" 
--                         ++ " to be type Boolean to branch! Attempted type: " ++ xType) state

-- --Pattern matches function def and call.
-- doNode (Expression((Function {funcCmd = cmd, funcName = name, funcBod = body}):rest)) state =
--     case (astNodeToString cmd) of 
--         "def" -> 
--             let fName = astNodeToString name
--             in case (M.lookup fName (fns state)) of 
--                     Just bod -> throwError ("Function Def Error. Function " ++ fName ++ " already exists!") state
--                     Nothing ->
--                         let fns' = M.insert fName body (fns state)
--                         in doNode (Expression rest) EDState{stack = (stack state), fns = fns', vars = (vars state), frames = (frames state), heap = heap state}

--         "call" -> 
--             let fName = astNodeToString name
--             in case (M.lookup fName (fns state)) of
--                 Just bod -> 
--                     (doNode bod (addFrame state)) >>= (\state' -> doNode (Expression rest) state')
                
--                 Nothing -> throwError ("Function Call Error. Function " ++ fName ++ " isn't defined!") state

--         other -> throwError ("Function Error. Invalid function command given. Given: " ++ other ++ " Valid: def, call") state

-- --Runs all the different cases of variable actions.
-- doNode (Expression((Variable{varName = name, varCmd = cmd}):rest)) state =
--     case (astNodeToString cmd) of
--         --Used in making a new variable in vars.
--         "mak" ->
--             let vName = astNodeToString name
--             in if (null $ stack state)
--                 then 
--                     throwError ("Variable (var) Mak Error. Can't create variable when stack is empty. Attempted variable name: " ++ vName) state 
--                 else
--                     case (M.lookup vName (vars state)) of
--                         Just _ -> throwError ("Variable (var) Mak Error. Variable " ++ vName ++ " already exists.") state
--                         Nothing -> 
--                             let vars' = M.insert vName (fsTop state) (vars state)
--                             in doNode (Expression rest) EDState{stack = (stack state), fns = (fns state), vars = vars', frames = (frames state), heap = heap state}
                           
--         --Pushes variable value to stack.
--         "get" -> 
--             let vName = astNodeToString name
--             in case (M.lookup vName (vars state)) of
--                 Just v -> doNode (Expression rest) (fsPush v state)
--                 Nothing -> throwError ("Variable (var) Get Error. Variable " ++ vName ++ " doesn't exist or was deleted!") state

--         --Removes variable from existence since var is manually scoped.
--         "del" -> 
--             let vName = astNodeToString name
--             in case (M.lookup vName (vars state)) of 
--                 Just v -> 
--                     let vars' = M.delete vName (vars state)
--                     in doNode (Expression rest) EDState{stack = (stack state), fns = (fns state), vars = vars', frames = (frames state), heap = heap state} 
--                 Nothing -> throwError ("Variable (var) Del Error. Variable " ++ vName ++ " doesn't exist or was already deleted!") state

--         --Alters variable to new value on top of stack if the types match.
--         "mut" -> 
--             let vName = astNodeToString name
--             in if (null $ stack state)
--                 then
--                     throwError ("Variable (var) Mut Error. Can't mutate variable when stack is empty! Attempted variable name: " ++ vName) state
--                 else
--                     let newVal = fsTop state
--                     in case (M.lookup vName (vars state)) of 
--                         Just v -> 
--                             if (compareTypesForMut v newVal)
--                                 then 
--                                     let vars' = M.insert vName newVal (vars state)
--                                     in doNode (Expression rest) EDState{stack = (stack state), fns = (fns state), vars = vars', frames = (frames state), heap = heap state}
--                                 else
--                                     throwError ("Variable (var) Mut Error. Can't mutate variable " 
--                                         ++ vName ++ " of type " ++ (chrs $ doQueryType' v) 
--                                         ++ " to different type: " ++ (chrs $ doQueryType' newVal)) state

--                         Nothing -> throwError ("Variable (var) Mut Error. Variable " ++ vName ++ " doesn't exist or was deleted!") state

--         other -> throwError ("Variable Command Error. Invalid variable command given! Given: " ++ other ++ " Valid: mak, get, mut, del") state

-- --Runs all the different cases of local variable actions.
-- doNode (Expression((LocVar{name = name, cmd = cmd}):rest)) state =
--     case (astNodeToString cmd) of
--         "mak" ->
--             let vName = astNodeToString name
--             in if (null $ stack state)
--                 then throwError ("Local Variable (loc) Mak Error. Can't create local variable when stack is empty! Attempted local variable name: " ++ vName) state 
--                 else
--                     case (M.lookup vName (head $ frames state)) of
--                         Just _ -> throwError ("Local Variable (loc) Mak Error. Local variable " ++ vName ++ " already exists in current scope.") state
--                         Nothing ->
--                             let frame' = M.insert vName (fsTop state) (head $ frames state)
--                             in doNode (Expression rest) EDState{stack = (stack state), fns = (fns state), vars = (vars state), frames = (frame' : (tail $ frames state)), heap = heap state}
                           
--         "get" ->  
--             case (getLoc (frames state) (astNodeToString name)) of
--                 Just value -> doNode (Expression rest) (fsPush value state)
--                 Nothing -> throwError ("Local Variable (loc) Get Error. Local Variable " ++ (astNodeToString name) ++ " not defined in any scope!") state

--         "mut" -> 
--             let vName = astNodeToString name
--             in if (null $ stack state)
--                 then throwError ("Local Variable (loc) Mut Error. Can't mutate local variable when stack is empty! Attempted local variable name: " ++ vName) state
--                 else
--                     case (getLoc (frames state) vName) of
--                         Just v -> 
--                             if (compareTypesForMut (fsTop state) v)
--                                 then
--                                     doNode (Expression rest) EDState{stack = (stack state), fns = (fns state), vars = (vars state), frames = (updateFrames (frames state) [] (fsTop state) vName False), heap = heap state}
--                                 else
--                                     throwError ("Local Variable (loc) Mut Error. Can't mutate local variable " 
--                                         ++ vName ++ " of type " ++ (chrs $ doQueryType' v) 
--                                         ++ " to different type: " ++ (chrs $ doQueryType' (fsTop state))) state

--                         Nothing -> throwError ("Local Variable (loc) Mut Error. Local Variable " ++ vName ++ " not defined for mutation in any scope!") state

--         other -> throwError ("Local Variable (loc) Command Error. Invalid local variable command given! Given: " ++ other ++ " valid: mak, get, mut") state

-- --This code looks sus but basically it runs the loop body 
-- -- and then runs the whole loop recursion if it needs to run again.                                                                                                                      
-- doNode ( While loopBody ) state = 
--     if (null $ stack state)
--         then throwError "While Loop Error. No Boolean value for while loop to check because stack is empty!" state
--         else do 
--             newState <- case (fsTop state) of 
--                 Boolean True -> doNode loopBody (addFrame state)
--                 Boolean False -> return state
--                 x -> 
--                     let xType = chrs $ doQueryType' x 
--                     in throwError ("While Loop Error. Top of stack needs to be type Boolean for loop to try and run! Attempted type: " 
--                         ++ xType) state

--             --Checks to see if loop can run again or not.
--             if (null $ stack newState)
--                 then throwError "While Loop Error. No Boolean value for while loop to check because stack is empty!" state
--                 else 
--                     case (fsTop newState) of 
--                         Boolean True -> doNode (While loopBody) newState
--                         Boolean False -> return newState
--                         x -> 
--                             let xType = chrs $ doQueryType' x 
--                             in throwError ("While Loop Error. Top of stack needs to be type Boolean for loop to try and run! Attempted type: " 
--                                 ++ xType) newState

-- -- doing a terminal changes depending on whether it's a word or a number. 
-- -- if it's a number, push it...
-- doNode ( Terminal ( Val v ) ) state = return $ fsPush v state

-- -- ...if it's a word, execute the operation
-- doNode ( Terminal ( Word o ) ) state = doOp o state

-- -- "doing" an empty expression does nothing
-- doNode ( Expression [] ) state = return $ removeFrame state

-- -- "doing" a non-empty expression tries to execute every node in the expression
-- doNode ( Expression ( first:rest ) ) state = do  
--     stateAfterFirst <- doNode first state
--     doNode (Expression (rest)) stateAfterFirst 

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

makeLine :: Int -> [String] -> String
makeLine indent strs = intercalate "" ((nFourSpaces indent):strs)

--This function is seperate from the main blocks because it'll be called twice, 
-- since both "push" and "p" are valid to push stuff to lists.
makeListPushCode :: Int -> Int -> ([String], Int)
makeListPushCode indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines = 
            [
                makeLine indent ["let (", stateStr, "', secondToTop, top) = pop2 ", stateStr],
                makeLine indent ["newState <- case (secondToTop, top) of"],
                makeLine (indent + 1) ["(Just (List{items = is, len = l}), Just v) -> return $ push ", stateStr, "' (List{items = M.insert l v is, len = l + 1})"],
                makeLine (indent + 1) ["(Just (String{chrs = cs, len = l}), Just (Char c)) -> return $ push ", stateStr, "' (String{chrs = cs ++ [c], len = l + 1})"],
                makeLine (indent + 1) ["(Just (String{chrs = cs, len = l}), Just v) -> let vType = chrs $ doQueryType' v in throwError (\"Operator (push) error.\ 
                \ Push operator needs a List/String and a Value/Char to be pushed. Attempted types: String and \" ++ vType) ", stateStr],
                makeLine (indent + 1) ["(Just v1, Just v2) -> let (v1Type, v2Type) = findTypeStrsForError v1 v2 in throwError (\"Operator (push) error.\ 
                \ Push operator needs a List/String and a Value/Char to be pushed. Attempted types: \" ++ v1Type ++ \" and \" ++ v2Type) ", stateStr],
                makeLine (indent + 1) ["(Nothing, Just v2) -> throwError \"Operator (push) error. Two operands required for push; only one provided!\" ", stateStr],
                makeLine (indent + 1) ["(Nothing, Nothing) -> throwError \"Operator (push) error. Two operands required for push; none provided!\" ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)

makeListPopCode :: Int -> Int -> ([String], Int)
makeListPopCode indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        stateStr' = stateStr ++ "'"
        codeLines =
            [
                makeLine indent ["let (", stateStr', ", top) = pop ", stateStr],
                makeLine indent ["newState <- case top of"],
                makeLine (indent + 1) ["Just (List{items = _, len = 0}) -> return ", stateStr],
                makeLine (indent + 1) ["Just (List{items = is, len = l}) -> \
                \let popped = fromJust (M.lookup (l - 1) is) ; is' = M.delete (l - 1) is \
                \in return $ push (push ", stateStr', "(List{items = is', len = l - 1})) (popped)"],
                makeLine (indent + 1) ["Just (String{chrs = \"\", len = 0}) -> return ", stateStr],
                makeLine (indent + 1) ["Just (String{chrs = cs, len = l}) -> let newStr = String{chrs = init cs, len = l - 1} ; popped = Char $ last cs \
                \in return $ push (push ", stateStr', " (newStr)) popped"],
                makeLine (indent + 1) ["Just v -> let vType = chrs $ doQueryType' v in throwError (\"Operator (pop) error.\
                \ Pop operator needs a List/String to pop items on top of stack. Attempted type: \" ++ vType) ", stateStr'],
                makeLine (indent + 1) ["Nothing -> throwError \"Operator (pop). error. Pop operator needs one operand; none provided!\" ", stateStr'],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)

createListFrontPushCode :: Int -> Int -> ([String], Int)
createListFrontPushCode indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        stateStr' = stateStr ++ "'"
        codeLines =
            [
                makeLine indent ["let (", stateStr, "', secondToTop, top) = pop2 ", stateStr],
                makeLine indent ["newState <- case (secondToTop, top) of"],
                
                makeLine (indent + 1) ["(Just (List{items = is, len = l}), Just v) -> ",
                    "let updateList = (\\List{items = is, len = l} index insVal -> \
                    \if index < l \
                        \then let old = fromJust $ M.lookup index is ; \
                        \is' = M.insert index insVal is \
                        \in updateList List{items = is', len = l} (index + 1) old " ,
                    "else let is' = M.insert index insVal is in List{items = is', len = l + 1}",
                    " ) ; ",
                    "newList = updateList List{items = is, len = l} 0 v ",
                    "in return $ push ", stateStr', " newList" 
                ],

                makeLine (indent + 1) ["(Just (String{chrs = cs, len = l}), Just (Char c)) -> return $ push ", stateStr, "' (String{chrs = (c:cs), len = l + 1})"],
                makeLine (indent + 1) ["(Just (String{chrs = cs, len = l}), Just v) -> let vType = chrs $ doQueryType' v in throwError (\"Operator (fpush) error.\ 
                \ Operator fpush needs a List/String and a Value/Char to be pushed to front. Attempted types: String and \" ++ vType) ", stateStr],
                makeLine (indent + 1) ["(Just v1, Just v2) -> let (v1Type, v2Type) = findTypeStrsForError v1 v2 in throwError (\"Operator (fpush) error.\ 
                \ Operator fpush needs a List/String and a Value/Char to be pushed to front. Attempted types: \" ++ v1Type ++ \" and \" ++ v2Type) ", stateStr],
                makeLine (indent + 1) ["(Nothing, Just v2) -> throwError \"Operator (fpush) error. Two operands required for fpush; only one provided!\" ", stateStr],
                makeLine (indent + 1) ["(Nothing, Nothing) -> throwError \"Operator (fpush) error. Two operands required for fpush; none provided!\" ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)

generateLengthCode :: Int -> Int -> ([String], Int)
generateLengthCode indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        stateStr' = stateStr ++ "'"
        codeLines = 
            [
                makeLine indent ["let (", stateStr', ", top) = pop ", stateStr],
                makeLine indent ["newState <- case top of"],
                makeLine (indent + 1) ["Just List{items = _, len = l} -> return $ push ", stateStr, " (Integer l)"],
                makeLine (indent + 1) ["Just String{chrs = _, len = l} -> return $ push ", stateStr, " (Integer l)"],
                makeLine (indent + 1) ["Just v -> let vType = chrs $ doQueryType' v in \
                \throwError (\"Operator (length) error. List/String type is needed for length operator to work. Attempted type: \" ++ vType) ", stateStr],
                makeLine (indent + 1) ["Nothing -> throwError (\"Operator (length) error. Operand needed for length; none provided!\") ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ] 
    in (codeLines, stateCount + 1)

createListFrontPopCode :: Int -> Int -> ([String], Int)
createListFrontPopCode indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        stateStr' = stateStr ++ "'"
        codeLines =
            [
                makeLine indent ["let (", stateStr', ", top) = pop ", stateStr],
                makeLine indent ["newState <- case top of"],
                makeLine (indent + 1) ["Just (List{items = _, len = 0}) -> return ", stateStr],
                makeLine (indent + 1) ["Just (List{items = is, len = l}) -> ",
                    "let popped = fromJust $ M.lookup 0 is ; ",
                    "updateList = (\\List{items = as, len = al} List{items = bs, len = bl} index -> ",
                    "if index < al then let ins = fromJust $ M.lookup index as ; bs' = M.insert (index - 1) ins bs ",
                    "in updateList List{items = as, len = al} List{items = bs', len = bl + 1} (index + 1)",
                    " else List{items = bs, len = bl}", 
                    ") ; ",
                    "newLs = updateList List{items = is, len = l} List{items = M.empty, len = 0} 1 ; ",
                    "in return $ push (push ", stateStr', " newLs) popped"
                ],
                makeLine (indent + 1) ["Just (String{chrs = \"\", len = 0}) -> return ", stateStr],
                makeLine (indent + 1) ["Just (String{chrs = cs, len = l}) -> let popped = Char $ head cs ; newStr = String{chrs = tail cs, len = l - 1} in \
                \return $ push (push ", stateStr', " newStr) popped"],
                makeLine (indent + 1) ["Just v -> let vType = chrs $ doQueryType' v in throwError (\"Operator (fpop) error. \
                \Popping from front requires a List/String to pop from. Attempted type: \" ++ vType) ", stateStr'],
                makeLine (indent + 1) ["Nothing -> throwError \"Operator (fpop) error. Needs one operand to work; none provided!\" ", stateStr'],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)

generateOpCode :: String -> Int -> Int -> ([String], Int)
generateOpCode "+" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines = 
            [
                makeLine indent ["let (", stateStr, "'", ", ", "secondToTop, ", "top) = pop2 ", stateStr],
                makeLine indent ["newState <- case (secondToTop, top) of"],
                makeLine (indent + 1) ["(Just v1, Just v2) -> "],
                makeLine (indent + 2) ["case (addVals v1 v2) of"],
                makeLine (indent + 3) ["Left v -> return $ push ", stateStr, "' (v)"],
                makeLine (indent + 3) ["Right err -> throwError err ", stateStr],
                makeLine (indent + 1) ["(Nothing, Just v2) -> ", "throwError \"Operator (+) error. Addition requires two operands; only one provided!\" ", stateStr],
                makeLine (indent + 1) ["(Nothing, Nothing) -> throwError \"Operator (+) error. Addition requires two operands; none provided!\" ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)
generateOpCode "-" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines = 
            [
                makeLine indent ["let (", stateStr, "'", ", ", "secondToTop, ", "top) = pop2 ", stateStr],
                makeLine indent ["newState <- case (secondToTop, top) of"],
                makeLine (indent + 1) ["(Just v1, Just v2) -> "],
                makeLine (indent + 2) ["case (subVals v2 v1) of"],
                makeLine (indent + 3) ["Left v -> return $ push ", stateStr, "' (v)"],
                makeLine (indent + 3) ["Right err -> throwError err ", stateStr],
                makeLine (indent + 1) ["(Nothing, Just v2) -> ", "throwError \"Operator (-) error. Subtraction requires two operands; only one provided!\" ", stateStr],
                makeLine (indent + 1) ["(Nothing, Nothing) -> throwError \"Operator (-) error. Subtraction requires two operands; none provided!\" ", stateStr],
                makeLine (indent) ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)
generateOpCode "*" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines = 
            [
                makeLine indent ["let (", stateStr, "'", ", ", "secondToTop, ", "top) = pop2 ", stateStr],
                makeLine indent ["newState <- case (secondToTop, top) of"],
                makeLine (indent + 1) ["(Just v1, Just v2) -> "],
                makeLine (indent + 2) ["case (multVals v1 v2) of"],
                makeLine (indent + 3) ["Left v -> return $ push ", stateStr, "' (v)"],
                makeLine (indent + 3) ["Right err -> throwError err ", stateStr],
                makeLine (indent + 1) ["(Nothing, Just v2) -> ", "throwError \"Operator (*) error. Multiplication requires two operands; only one provided!\" ", stateStr],
                makeLine (indent + 1) ["(Nothing, Nothing) -> throwError \"Operator (*) error. Multiplication requires two operands; none provided!\" ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)
generateOpCode "/" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines = 
            [
                makeLine indent ["let (", stateStr, "'", ", ", "secondToTop, ", "top) = pop2 ", stateStr],
                makeLine indent ["newState <- case (secondToTop, top) of"],
                makeLine (indent + 1) ["(Just v1, Just v2) -> "],
                makeLine (indent + 2) ["case (divideVals v2 v1) of"],
                makeLine (indent + 3) ["Left v -> return $ push ", stateStr, "' (v)"],
                makeLine (indent + 3) ["Right err -> throwError err ", stateStr],
                makeLine (indent + 1) ["(Nothing, Just v2) -> ", "throwError \"Operator (/) error. Division requires two operands; only one provided!\" ", stateStr],
                makeLine (indent + 1) ["(Nothing, Nothing) -> throwError \"Operator (/) error. Division requires two operands; none provided!\" ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)

generateOpCode "swap" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines =
            [
                makeLine indent ["let (", stateStr, "', ", "secondToTop, top) = pop2 ", stateStr],
                makeLine indent ["newState <- case (secondToTop, top) of"],
                makeLine (indent + 1) ["(Just v1, Just v2) -> return $ push (", "push ", stateStr, "' (v2)) (v1)"],
                makeLine (indent + 1) ["(Nothing, Just v2) -> return ", stateStr],
                makeLine (indent + 1) ["(Nothing, Nothing) -> return ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)
generateOpCode "drop" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines =
            [
                makeLine indent ["let state", show $ stateCount + 1, " = fst $ pop ", stateStr]
            ]
    in (codeLines, stateCount + 1)
generateOpCode "dropStack" indent stateCount =
    let codeLines = 
            [
                makeLine indent ["let state", show $ stateCount + 1, " = EDState{stack = []}"] --Will need to change this a bit to accommodate for transferring information from previous state. 
            ]
    in (codeLines, stateCount + 1)
generateOpCode "rot" indent stateCount = 
    let stateStr = "state" ++ (show stateCount)
        codeLines =
            [
                makeLine indent ["let (", stateStr, "', thirdToTop, secondToTop, top) = pop3 ", stateStr],
                makeLine indent ["newState <- case (thirdToTop, secondToTop, top) of"],
                makeLine (indent + 1) ["(Just v1, Just v2, Just v3) -> return $ push (", "push (", "push ", stateStr, "' (v3)) (v1)) (v2)"],
                makeLine (indent + 1) ["(Nothing, Just v2, Just v3) -> return $ push (", "push ", stateStr, "' (v3)) (v2)"],
                makeLine (indent + 1) ["(Nothing, Nothing, Just v3) -> return $ ", stateStr],
                makeLine (indent + 1) ["(Nothing, Nothing, Nothing) -> return $ ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)
generateOpCode "dup" indent stateCount =
    let codeLines = 
            [
                makeLine indent ["let (_, top) = pop state", show stateCount],
                makeLine indent ["newState <- case top of"],
                makeLine (indent + 1) ["Just v -> return $ push state", show stateCount, "(v)"],
                makeLine (indent + 1) ["Nothing -> return state", show stateCount],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)
generateOpCode "==" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines =
            [
                makeLine indent ["let (", stateStr, "', secondToTop, top) = pop2 ", stateStr],
                makeLine indent ["newState <- case (secondToTop, top) of"],
                makeLine (indent + 1) ["(Just v1, Just v2) ->"],
                makeLine (indent + 2) ["case (doEqual v1 v2) of"],
                makeLine (indent + 3) ["Left v -> return $ push ", stateStr, "' (v)"],
                makeLine (indent + 3) ["Right err -> throwError err ", stateStr, "'"],
                makeLine (indent + 1) ["(Nothing, Just v2) -> throwError \"Operator (==) error. Equality comparison requires two operands; only one provided!\"", stateStr, "'"],
                makeLine (indent + 1) ["(Nothing, Nothing) -> throwError \"Operator (==) error. Equality comparison requires two operands; none provided!\"", stateStr, "'"],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)
generateOpCode "/=" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines =
            [
                makeLine indent ["let (", stateStr, "', secondToTop, top) = pop2 ", stateStr],
                makeLine indent ["newState <- case (secondToTop, top) of"],
                makeLine (indent + 1) ["(Just v1, Just v2) ->"],
                makeLine (indent + 2) ["case (doNotEqual v1 v2) of"],
                makeLine (indent + 3) ["Left v -> return $ push ", stateStr, "' (v)"],
                makeLine (indent + 3) ["Right err -> throwError err ", stateStr, "'"],
                makeLine (indent + 1) ["(Nothing, Just v2) -> throwError \"Operator (/=) error. Inequality comparison requires two operands; only one provided!\"", stateStr, "'"],
                makeLine (indent + 1) ["(Nothing, Nothing) -> throwError \"Operator (/=) error. Inequality comparison requires two operands; none provided!\"", stateStr, "'"],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)
generateOpCode ">" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines =
                [
                    makeLine indent ["let (", stateStr, "', secondToTop, top) = pop2 ", stateStr],
                    makeLine indent ["newState <- case (secondToTop, top) of"],
                    makeLine (indent + 1) ["(Just v1, Just v2) -> "],
                    makeLine (indent + 2) ["case (doGreaterThan v1 v2) of"],
                    makeLine (indent + 3) ["Left v -> return $ push ", stateStr, "' (v)"],
                    makeLine (indent + 3) ["Right err -> throwError err ", stateStr, "'"],
                    makeLine (indent + 1) ["(Nothing, Just v2) -> throwError \"Operator (>) error. Greater than comparison requires two operands; only one provided!\" ", stateStr, "'"],
                    makeLine (indent + 1) ["(Nothing, Nothing) -> throwError \"Operator (>) error. Greater than comparison requires two operands; none provided!\" ", stateStr, "'"],
                    makeLine indent ["let state", show $ stateCount + 1, " = newState"]
                ]
    in (codeLines, stateCount + 1)
generateOpCode "<" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines =
                [
                    makeLine indent ["let (", stateStr, "', secondToTop, top) = pop2 ", stateStr],
                    makeLine indent ["newState <- case (secondToTop, top) of"],
                    makeLine (indent + 1) ["(Just v1, Just v2) -> "],
                    makeLine (indent + 2) ["case (doLessThan v1 v2) of"],
                    makeLine (indent + 3) ["Left v -> return $ push ", stateStr, "' (v)"],
                    makeLine (indent + 3) ["Right err -> throwError err ", stateStr, "'"],
                    makeLine (indent + 1) ["(Nothing, Just v2) -> throwError \"Operator (<) error. Less than comparison requires two operands; only one provided!\" ", stateStr, "'"],
                    makeLine (indent + 1) ["(Nothing, Nothing) -> throwError \"Operator (<) error. Less than comparison requires two operands; none provided!\" ", stateStr, "'"],
                    makeLine (indent) ["let state", show $ stateCount + 1, " = newState"]
                ]
    in (codeLines, stateCount + 1)
generateOpCode ">=" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines =
                [
                    makeLine indent ["let (", stateStr, "', secondToTop, top) = pop2 ", stateStr],
                    makeLine indent ["newState <- case (secondToTop, top) of"],
                    makeLine (indent + 1) ["(Just v1, Just v2) -> "],
                    makeLine (indent + 2) ["case (doGreaterThanEqualTo v1 v2) of"],
                    makeLine (indent + 3) ["Left v -> return $ push ", stateStr, "' (v)"],
                    makeLine (indent + 3) ["Right err -> throwError err ", stateStr, "'"],
                    makeLine (indent + 1) ["(Nothing, Just v2) -> throwError \"Operator (>=) error. Greater than equal to comparison requires two operands; only one provided!\" ", stateStr, "'"],
                    makeLine (indent + 1) ["(Nothing, Nothing) -> throwError \"Operator (>=) error. Greater than equal to comparison requires two operands; none provided!\" ", stateStr, "'"],
                    makeLine indent ["let state", show $ stateCount + 1, " = newState"]
                ]
    in (codeLines, stateCount + 1)
generateOpCode "<=" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines =
                [
                    makeLine indent ["let (", stateStr, "', secondToTop, top) = pop2 ", stateStr],
                    makeLine indent ["newState <- case (secondToTop, top) of"],
                    makeLine (indent + 1) ["(Just v1, Just v2) -> "],
                    makeLine (indent + 2) ["case (doLessThanEqualTo v1 v2) of"],
                    makeLine (indent + 3) ["Left v -> return $ push ", stateStr, "' (v)"],
                    makeLine (indent + 3) ["Right err -> throwError err ", stateStr, "'"],
                    makeLine (indent + 1) ["(Nothing, Just v2) -> throwError \"Operator (<=) error. Less than equal to comparison requires two operands; only one provided!\" ", stateStr, "'"],
                    makeLine (indent + 1) ["(Nothing, Nothing) -> throwError \"Operator (<=) error. Less than equal to comparison requires two operands; none provided!\" ", stateStr, "'"],
                    makeLine indent ["let state", show $ stateCount + 1, " = newState"]
                ]
    in (codeLines, stateCount + 1)

generateOpCode "%" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines = 
            [
                makeLine indent ["let (", stateStr, "'", ", ", "secondToTop, ", "top) = pop2 ", stateStr],
                makeLine indent ["newState <- case (secondToTop, top) of"],
                makeLine (indent + 1) ["(Just v1, Just v2) -> "],
                makeLine (indent + 2) ["case (doModulo v1 v2) of"],
                makeLine (indent + 3) ["Left v -> return $ push ", stateStr, "' (v)"],
                makeLine (indent + 3) ["Right err -> throwError err ", stateStr],
                makeLine (indent + 1) ["(Nothing, Just v2) -> ", "throwError \"Operator (%) error. Modulo requires two operands; only one provided!\" ", stateStr],
                makeLine (indent + 1) ["(Nothing, Nothing) -> throwError \"Operator (%) error. Modulo requires two operands; none provided!\" ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)
generateOpCode "++" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines = 
            [
                makeLine indent ["let (", stateStr, "'", ", ", "secondToTop, ", "top) = pop2 ", stateStr],
                makeLine indent ["newState <- case (secondToTop, top) of"],
                makeLine (indent + 1) ["(Just v1, Just v2) -> "],
                makeLine (indent + 2) ["case (doConcat v1 v2) of"],
                makeLine (indent + 3) ["Left v -> return $ push ", stateStr, "' (v)"],
                makeLine (indent + 3) ["Right err -> throwError err ", stateStr],
                makeLine (indent + 1) ["(Nothing, Just v2) -> ", "throwError \"Operator (++) error. Concatenation requires two operands; only one provided!\" ", stateStr],
                makeLine (indent + 1) ["(Nothing, Nothing) -> throwError \"Operator (++) error. Concatenation requires two operands; none provided!\" ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)
generateOpCode "and" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines =
                [
                    makeLine indent ["let (", stateStr, "', secondToTop, top) = pop2 ", stateStr],
                    makeLine indent ["newState <- case (secondToTop, top) of"],
                    makeLine (indent + 1) ["(Just (Boolean b1), Just (Boolean b2)) -> return $ push ", stateStr, "' (Boolean $ b1 && b2)"],
                    makeLine (indent + 1) ["(Just v1, Just v2) -> let (v1Type, v2Type) = findTypeStrsForError v1 v2 in throwError (\"Operator (and) error. \
                    \Can't logically AND two items that are not both types of Boolean! Attempted types were: \" ++ v1Type ++ \" and \" ++ v2Type)", stateStr],
                    makeLine (indent + 1) ["(Nothing, Just v2) -> throwError \"Operator (and) error. Logical AND requires two operands; only one provided!\" ", stateStr],
                    makeLine (indent + 1) ["(Nothing, Nothing) -> throwError \"Operator (and) error. Logical AND requires two operands; none provided!\" ", stateStr],
                    makeLine indent ["let state", show $ stateCount + 1, " = newState"]
                ]
    in (codeLines, stateCount + 1)
generateOpCode "or" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines =
                [
                    makeLine indent ["let (", stateStr, "', secondToTop, top) = pop2 ", stateStr],
                    makeLine indent ["newState <- case (secondToTop, top) of"],
                    makeLine (indent + 1) ["(Just (Boolean b1), Just (Boolean b2)) -> return $ push ", stateStr, "' (Boolean $ b1 || b2)"],
                    makeLine (indent + 1) ["(Just v1, Just v2) -> let (v1Type, v2Type) = findTypeStrsForError v1 v2 in throwError (\"Operator (or) error. \
                    \Can't logically OR two items that are not both types of Boolean! Attempted types were: \" ++ v1Type ++ \" and \" ++ v2Type)", stateStr],
                    makeLine (indent + 1) ["(Nothing, Just v2) -> throwError \"Operator (or) error. Logical OR requires two operands; only one provided!\" ", stateStr],
                    makeLine (indent + 1) ["(Nothing, Nothing) -> throwError \"Operator (or) error. Logical OR requires two operands; none provided!\" ", stateStr],
                    makeLine indent ["let state", show $ stateCount + 1, " = newState"]
                ]
    in (codeLines, stateCount + 1)
generateOpCode "xor" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines =
                [
                    makeLine indent ["let (", stateStr, "', secondToTop, top) = pop2 ", stateStr],
                    makeLine indent ["newState <- case (secondToTop, top) of"],
                    makeLine (indent + 1) ["(Just (Boolean b1), Just (Boolean b2)) -> return $ push ", stateStr, "' (Boolean $ (b1 /= b2))"],
                    makeLine (indent + 1) ["(Just v1, Just v2) -> let (v1Type, v2Type) = findTypeStrsForError v1 v2 in throwError (\"Operator (xor) error. \
                    \Can't logically XOR two items that are not both types of Boolean! Attempted types were: \" ++ v1Type ++ \" and \" ++ v2Type)", stateStr],
                    makeLine (indent + 1) ["(Nothing, Just v2) -> throwError \"Operator (xor) error. Logical XOR requires two operands; only one provided!\" ", stateStr],
                    makeLine (indent + 1) ["(Nothing, Nothing) -> throwError \"Operator (xor) error. Logical XOR requires two operands; none provided!\" ", stateStr],
                    makeLine indent ["let state", show $ stateCount + 1, " = newState"]
                ]
    in (codeLines, stateCount + 1)
generateOpCode "not" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines =
                [
                    makeLine indent ["let (", stateStr, "', top) = pop ", stateStr],
                    makeLine indent ["newState <- case (top) of"],
                    makeLine (indent + 1) ["(Just (Boolean b1)) -> return $ push ", stateStr, "' (Boolean $ (not b1))"],
                    makeLine (indent + 1) ["(Just v1) -> let (v1Type) = chrs $ doQueryType' v1 in throwError (\"Operator (not) error. \
                    \Can't logically NOT item that isn't type Boolean! Attempted type was: \" ++ v1Type) ", stateStr],
                    makeLine (indent + 1) ["(Nothing) -> throwError \"Operator (not) error. Logical NOT operation requires one operand; none provided!\" ", stateStr],
                    makeLine indent ["let state", show $ stateCount + 1, " = newState"]
                ]
    in (codeLines, stateCount + 1)
generateOpCode "pow" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines =
                [
                    makeLine indent ["let (", stateStr, "', secondToTop, top) = pop2 ", stateStr],
                    makeLine indent ["newState <- case (secondToTop, top) of"],
                    makeLine (indent + 1) ["(Just (Float f1), Just (Float f2)) -> return $ push ", stateStr, "' (Float $ f1 ** f2)"],
                    makeLine (indent + 1) ["(Just (Double d1), Just (Double d2)) -> return $ push ", stateStr, "' (Double $ d1 ** d2)"],
                    makeLine (indent + 1) ["(Just v1, Just v2) -> let (v1Type, v2Type) = findTypeStrsForError v1 v2 in throwError (\"Operator (pow) error. \
                    \Operands need to be both of type Float or Double! Attempted types: \" ++ v1Type ++ \" and \" ++ v2Type)", stateStr],
                    makeLine (indent + 1) ["(Nothing, Just v2) -> throwError \"Operator (pow) error. Two operands needed; only one provided!\" ", stateStr],
                    makeLine (indent + 1) ["(Nothing, Nothing) -> throwError \"Operator (pow) error. Two operands needed; none provided!\" ", stateStr],
                    makeLine indent ["let state", show $ stateCount + 1, " = newState"]
                ]
    in (codeLines, stateCount + 1)

generateOpCode "push" indent stateCount = makeListPushCode indent stateCount
generateOpCode "p" indent stateCount = makeListPushCode indent stateCount
generateOpCode "pop" indent stateCount = makeListPopCode indent stateCount
generateOpCode "po" indent stateCount = makeListPopCode indent stateCount
generateOpCode "fpush" indent stateCount = createListFrontPushCode indent stateCount
generateOpCode "fp" indent stateCount = createListFrontPushCode indent stateCount
generateOpCode "fpop" indent stateCount = createListFrontPopCode indent stateCount
generateOpCode "fpo" indent stateCount = createListFrontPopCode indent stateCount
generateOpCode "index" indent stateCount = 
    let stateStr = "state" ++ (show stateCount)
        stateStr' = stateStr ++ "'"
        codeLines = 
            [
                makeLine indent ["let (", stateStr', ", secondToTop, top) = pop2 ", stateStr],
                makeLine indent ["newState <- case (secondToTop, top) of"],
                makeLine (indent + 1) ["(Just (List{items = is, len = l}), Just (Integer idx)) -> case (M.lookup idx is) of "],
                makeLine (indent + 2) ["Just v -> return $ push (push ", stateStr', " (List{items = is, len = l})) v"],
                makeLine (indent + 2) ["Nothing -> throwError (\"Operator (index) error. Index \" ++ (show idx) ++ \" out of valid range for List of size \" ++ (show l) ++ \"!\") ", stateStr'],
                makeLine (indent + 1) ["(Just (String{chrs = cs, len = l}), Just (Integer idx)) -> if idx > -1 && idx < l \
                    \then return $ push (push ", stateStr', " String{chrs = cs, len = l}) (Char $ cs !! idx) ",
                    "else throwError (\"Operator (index) error. Index \" ++ (show idx) ++ \" out of valid range for String of size \" ++ (show l) ++ \"!\") ", stateStr'],
                makeLine (indent + 1) ["(Just v1, Just v2) -> let (v1Type, v2Type) = findTypeStrsForError v1 v2 \
                \in throwError (\"Operator (index) error. Index needs a List/String and an index value of type Integer! Attempted types: \" ++ v1Type ++ \" and \" ++ v2Type) ", stateStr'],
                makeLine (indent + 1) ["(Nothing, Just v2) -> throwError (\"Operator (index) error. Two operands required for index; only one provided!\") ", stateStr'],
                makeLine (indent + 1) ["(Nothing, Nothing) -> throwError (\"Operator (index) error. Two operands required for index; none provided!\") ", stateStr'],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)
generateOpCode "length" indent stateCount = generateLengthCode indent stateCount
generateOpCode "len" indent stateCount = generateLengthCode indent stateCount
generateOpCode "isEmpty" indent stateCount = 
    let stateStr = "state" ++ (show stateCount)
        stateStr' = stateStr ++ "'"
        codeLines =
            [
                makeLine indent ["let (", stateStr', ", top) = pop ", stateStr],
                makeLine indent ["newState <- case top of"],
                makeLine (indent + 1) ["Just (List{items = _, len = l}) -> return $ push ", stateStr, " (Boolean $ l == 0)"],
                makeLine (indent + 1) ["Just (String{chrs = _, len = l}) -> return $ push ", stateStr, " (Boolean $ l == 0)"],
                makeLine (indent + 1) ["Just (Object{fields = fs}) -> return $ push ", stateStr, " (Boolean $ M.null fs)"],
                makeLine (indent + 1) ["Just v -> let vType = chrs $ doQueryType' v \
                \in throwError (\"Operator (isEmpty) error. This operator is only valid for types of List/String/Object. Attempted type: \" ++ vType) ", stateStr],
                makeLine (indent + 1) ["Nothing -> throwError (\"Operator (isEmpty) error. One operand needed; none provided!\") ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)
generateOpCode "clear" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        stateStr' = stateStr ++ "'"
        codeLines =
            [
                makeLine indent ["let (", stateStr', ", top) = pop ", stateStr],
                makeLine indent ["newState <- case top of"],
                makeLine (indent + 1) ["Just (List{items = _, len = _}) -> return $ push ", stateStr', " (List{items = M.empty, len = 0})"],
                makeLine (indent + 1) ["Just (String{chrs = _, len = _}) -> return $ push ", stateStr', " (String{chrs = \"\", len = 0})"],
                makeLine (indent + 1) ["Just (Object{fields = fs}) -> return $ push ", stateStr', " (Object{fields = M.empty})"],
                makeLine (indent + 1) ["Just v -> let vType = chrs $ doQueryType' v \
                \in throwError (\"Operator (clear) error. Only type List/String/Object is valid for clear. Attempted type: \" ++ vType) ", stateStr],
                makeLine (indent + 1) ["Nothing -> throwError (\"Operator (clear) error. One operand needed; none provided!\") ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]   
            ]
    in (codeLines, stateCount + 1)
generateOpCode "contains" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        stateStr' = stateStr ++ "'"
        codeLines =
            [
                makeLine indent ["let (", stateStr', ", secondToTop, top) = pop2 ", stateStr],
                makeLine indent ["newState <- case (secondToTop, top) of"],
                makeLine (indent + 1) ["(Just (List{items = is, len = l}), Just v) -> return $ push ", stateStr, " (Boolean $ v `elem` is)"],
                makeLine (indent + 1) ["(Just (String{chrs = cs, len = l}), Just (Char c)) -> return $ push ", stateStr, " (Boolean $ c `elem` cs)"],
                makeLine (indent + 1) ["(Just (Object{fields = fs}), Just (String{chrs = name, len = l})) -> \
                \let contains = case (M.lookup name fs) of ; Just _ -> True ; Nothing -> False ; in return $ push ", stateStr, " (Boolean contains) "],
                makeLine (indent + 1) ["(Just v1, Just v2) -> let (v1Type, v2Type) = findTypeStrsForError v1 v2 \
                \in throwError (\"Operator (contains) error. First pushed element must be a List/String/Object \
                \and second item needs to be Value/Char/String respectively. Attempted types: \" ++ v1Type ++ \" and \" ++ v2Type) ", stateStr],
                makeLine (indent + 1) ["(Nothing, Just v2) -> throwError (\"Operator (contains) error. Two operands on stack needed; only one provided!\") ", stateStr],
                makeLine (indent + 1) ["(Nothing, Nothing) -> throwError (\"Operator (contains) error. Two operands on stack needed; none provided!\") ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)
generateOpCode "changeItemAt" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        stateStr' = stateStr ++ "'"
        codeLines = 
            [
                makeLine indent ["let (", stateStr', ", thirdToTop, secondToTop, top) = pop3 ", stateStr],
                makeLine indent ["newState <- case (thirdToTop, secondToTop, top) of"],
                makeLine (indent + 1) ["(Just (List{items = is, len = l}), Just v, Just (Integer idx)) -> "],
                makeLine (indent + 2) ["if (idx > -1 && idx < l) \
                    \then return $ push ", stateStr', "(List{items = M.insert idx v is, len = l}) \
                    \else throwError (\"Operator (changeItemAt) error. Index \" ++ (show idx) \
                    \++ \" out of range for List of size \" ++ (show l) ++ \"!\") ", stateStr'],
                makeLine (indent + 1) ["(Just v1, Just v2, Just v3) -> \
                \let (v1Type, v2Type, v3Type) = (chrs $ doQueryType' v1, chrs $ doQueryType' v2, chrs $ doQueryType' v3)\
                \ in throwError (\"Operator (changeItemAt) error. Top three items of stack need to be of type: \
                \List Value Integer (ordered from bottom to top). \
                \Attempted types: \" ++ v1Type ++ \", \" ++ v2Type ++ \", and \" ++ v3Type) ", stateStr],
                makeLine (indent + 1) ["(Nothing, Just v2, Just v3) -> throwError (\"Operator (changeItemAt) error. Three operands needed; only two provided!\") ", stateStr],
                makeLine (indent + 1) ["(Nothing, Nothing, Just v3) -> throwError (\"Operator (changeItemAt) error. Three operands needed; only one provided!\") ", stateStr],
                makeLine (indent + 1) ["(Nothing, Nothing, Nothing) -> throwError (\"Operator (changeItemAt) error. Three operands needed; none provided!\") ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]   
    in (codeLines, stateCount + 1)
generateOpCode "isWhitespace" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        stateStr' = stateStr ++ "'"
        codeLines = 
            [
                makeLine indent ["let (", stateStr', ", top) = pop ", stateStr],
                makeLine indent ["newState <- case top of"],
                makeLine (indent + 1) ["Just (Char c) -> return $ push ", stateStr, " (Boolean $ isSpace c) "],
                makeLine (indent + 1) ["Just v -> let vType = chrs $ doQueryType' v \
                \in throwError (\"Operator (isWhitespace) error. Type on stack top needs to be of type Char. Attempted type: \" ++ vType) ", stateStr],
                makeLine (indent + 1) ["Nothing -> throwError (\"Operator (isWhitespace) error. Operand on stack needed; none provided!\") ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)

generateOpCode "cast" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        stateStr' = stateStr ++ "'"
        codeLines = 
            [
                makeLine indent ["let (", stateStr', ", secondToTop, top) = pop2 ", stateStr],
                makeLine (indent) ["let castResult = case (secondToTop, top) of"],
                
                makeLine (indent + 2) ["(Just (Boolean b), Just (String{chrs = \"Integer\", len = _})) -> Left $ Integer $ if b then 1 else 0"],
                makeLine (indent + 2) ["(Just (Boolean b), Just (String{chrs = \"BigInteger\", len = _})) -> Left $ BigInteger $ if b then 1 else 0"],
                makeLine (indent + 2) ["(Just (Boolean b), Just (String{chrs = \"String\", len = _})) -> let boolStr = show b in Left $ String{chrs = boolStr, len = length boolStr}"],
                makeLine (indent + 2) ["(Just (Boolean b), Just (String{chrs = \"Boolean\", len = _})) -> Left $ Boolean b"],

                makeLine (indent + 2) ["(Just (BigInteger n), Just (String{chrs = \"String\", len = _})) -> let bigIntStr = show n in Left $ String{chrs = bigIntStr, len = length bigIntStr}"],
                makeLine (indent + 2) ["(Just (BigInteger n), Just (String{chrs = \"Integer\", len = _})) -> Left $ Integer (fromIntegral n :: Int)"],
                makeLine (indent + 2) ["(Just (BigInteger n), Just (String{chrs = \"BigInteger\", len = _})) -> Left $ BigInteger n"],
                makeLine (indent + 2) ["(Just (BigInteger n), Just (String{chrs = \"Float\", len = _})) -> Left $ Float (fromIntegral n :: Float)"],
                makeLine (indent + 2) ["(Just (BigInteger n), Just (String{chrs = \"Double\", len = _})) -> Left $ Double (fromIntegral n :: Double)"],
                makeLine (indent + 2) ["(Just (BigInteger n), Just (String{chrs = \"Char\", len = _})) -> let nInt = fromIntegral n :: Int \
                \in if validIntToChar nInt \
                    \then Left $ Char $ chr nInt \
                    \else Right (\"Operator (cast) error. Failed to convert type BigInteger to Char. Try making sure the Integer is in the UTF-8 numerical range.\" \
                    \++ \" Given value: \" ++ (show n) ++ \" valid numbers are \" ++ (show $ ord minBound) ++ \" to \" ++ (show $ ord maxBound) ++ \".\")"],

                makeLine (indent + 2) ["(Just (Integer n), Just (String{chrs = \"String\", len = _})) -> let intStr = show n in Left $ String{chrs = intStr, len = length intStr}"],
                makeLine (indent + 2) ["(Just (Integer n), Just (String{chrs = \"Integer\", len = _})) -> Left $ Integer n"],
                makeLine (indent + 2) ["(Just (Integer n), Just (String{chrs = \"BigInteger\", len = _})) -> Left $ BigInteger (fromIntegral n :: Integer)"],
                makeLine (indent + 2) ["(Just (Integer n), Just (String{chrs = \"Float\", len = _})) -> Left $ Float (fromIntegral n :: Float)"],
                makeLine (indent + 2) ["(Just (Integer n), Just (String{chrs = \"Double\", len = _})) -> Left $ Double (fromIntegral n :: Double)"],
                makeLine (indent + 2) ["(Just (Integer n), Just (String{chrs = \"Char\", len = _})) -> if validIntToChar n \
                    \then Left $ Char $ chr n \
                    \else Right (\"Operator (cast) error. Failed to convert type Integer to Char. Try making sure the Integer is in the UTF-8 numerical range. \
                    \Given value: \" ++ (show n) ++ \" valid numbers are \" ++ (show $ ord minBound) ++ \" to \" ++ (show $ ord maxBound) ++ \".\") "],

                makeLine (indent + 2) ["(Just (Float n), Just (String{chrs = \"String\", len = _})) -> let floatStr = show n in Left String{chrs = floatStr, len = length floatStr}"],
                makeLine (indent + 2) ["(Just (Float n), Just (String{chrs = \"Integer\", len = _})) -> Left $ Integer $ truncate n"],
                makeLine (indent + 2) ["(Just (Float n), Just (String{chrs = \"BigInteger\", len = _})) -> Left $ BigInteger (floor n :: Integer)"],
                makeLine (indent + 2) ["(Just (Float n), Just (String{chrs = \"Float\", len = _})) -> Left $ Float n"],
                makeLine (indent + 2) ["(Just (Float n), Just (String{chrs = \"Double\", len = _})) -> Left $ Double (realToFrac n :: Double)"],

                makeLine (indent + 2) ["(Just (Double n), Just (String{chrs = \"String\", len = _})) -> let dblStr = show n in Left String{chrs = dblStr, len = length dblStr}"],
                makeLine (indent + 2) ["(Just (Double n), Just (String{chrs = \"Integer\", len = _})) -> Left $ Integer (truncate n)"],
                makeLine (indent + 2) ["(Just (Double n), Just (String{chrs = \"BigInteger\", len = _})) -> Left $ BigInteger (floor n :: Integer)"],
                makeLine (indent + 2) ["(Just (Double n), Just (String{chrs = \"Float\", len = _})) -> Left $ Float (realToFrac n :: Float)"],
                makeLine (indent + 2) ["(Just (Double n), Just (String{chrs = \"Double\", len = _})) -> Left $ Double n"],

                makeLine (indent + 2) ["(Just (Char c), Just (String{chrs = \"String\", len = _})) -> let cStr = [c] in Left String{chrs = cStr, len = length cStr}"],
                makeLine (indent + 2) ["(Just (Char c), Just (String{chrs = \"Integer\", len = _})) -> Left $ Integer $ ord c"],
                makeLine (indent + 2) ["(Just (Char c), Just (String{chrs = \"BigInteger\", len = _})) -> Left $ BigInteger (fromIntegral (ord c) :: Integer)"],
                
                makeLine (indent + 2) ["(Just (String{chrs = cs, len = l}), Just (String{chrs = \"String\", len = _})) -> Left $ String{chrs = cs, len = l}"],
                makeLine (indent + 2) ["(Just (String{chrs = cs, len = l}), Just (String{chrs = \"Integer\", len = _})) -> case (readMaybe cs :: Maybe Int) of ; Just v -> Left $ Integer v ; \
                \Nothing -> Right (\"Operator (cast) error. Failed to convert String '\" ++ cs ++ \"' to type Integer.\")"],
                makeLine (indent + 2) ["(Just (String{chrs = cs, len = l}), Just (String{chrs = \"BigInteger\", len = _})) -> case (readMaybe cs :: Maybe Integer) of ; Just v -> Left $ BigInteger v ; \
                \Nothing -> Right (\"Operator (cast) error. Failed to convert String '\" ++ cs ++ \"' to type BigInteger.\")"],
                makeLine (indent + 2) ["(Just (String{chrs = cs, len = l}), Just (String{chrs = \"Float\", len = _})) -> case (readMaybe cs :: Maybe Float) of ; Just v -> Left $ Float v ; \
                \Nothing -> Right (\"Operator (cast) error. Failed to convert String '\" ++ cs ++ \"' to type Float.\")"],
                makeLine (indent + 2) ["(Just (String{chrs = cs, len = l}), Just (String{chrs = \"Double\", len = _})) -> case (readMaybe cs :: Maybe Double) of ; Just v -> Left $ Double v ; \
                \Nothing -> Right (\"Operator (cast) error. Failed to convert String '\" ++ cs ++ \"' to type Double.\")"],

                makeLine (indent + 2) ["(Just (List{items = is, len = l}), Just (String{chrs = \"String\", len = _})) -> \
                \let listStr = (\"[\" ++ (printList List{items = is, len = l} \"\" 0 False) ++ \"]\") ; listStrLen = length listStr ; in Left $ String{chrs = listStr, len = listStrLen}"],

                makeLine (indent + 2) ["(Just (Object{fields = fs}), Just (String{chrs = \"String\", len = _})) -> \
                \let objStr = (\"{\" ++ (printObj (M.toList fs) \"\") ++ \"}\") ; objStrLen = length objStr ; in Left $ String{chrs = objStr, len = objStrLen}"],

                makeLine (indent + 2) ["(Just (Box bn), Just (String{chrs = \"String\", len = _})) -> \
                \let boxStr = if bn == (-1) then \"Box NULL\" else \"Box \" ++ (show bn) in Left $ String{chrs = boxStr, len = length boxStr} "],
                makeLine (indent + 2) ["(Just (Box bn), Just (String{chrs = \"Integer\", len = _})) -> Left $ Integer bn"],
                makeLine (indent + 2) ["(Just (Box bn), Just (String{chrs = \"Boolean\", len = _})) -> Left $ Boolean $ bn /= (-1)"],

                makeLine (indent + 2) ["(Just (v), Just (String{chrs = typeCastStr, len = _})) -> let vType = chrs $ doQueryType' v \
                \in Right (\"Operator (cast) error. Invalid casting configuration given! Tried to cast \" ++ vType ++ \" to type \" ++ typeCastStr)"],
                makeLine (indent + 2) ["(Just v1, Just v2) -> let (v1Type, v2Type) = findTypeStrsForError v1 v2 \
                \in Right (\"Operator (cast) error. Types of Value and String required for cast to occur. Attempted types: \" ++ v1Type ++ \" and \" ++ v2Type)"],

                makeLine (indent + 2) ["(Nothing, Just v2) -> Right \"Operator (cast) error. Two operands required for cast; only one provided!\""],
                makeLine (indent + 2) ["(Nothing, Nothing) -> Right \"Operator (cast) error. Two operands required for cast; none provided!\""],

                makeLine (indent) ["newState <- case castResult of"],
                makeLine (indent + 1) ["Left v -> return $ push ", stateStr', " (v)"],
                makeLine (indent + 1) ["Right err -> throwError err ", stateStr'],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]

            ]
    in (codeLines, stateCount + 1)
generateOpCode "queryType" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        stateStr' = stateStr ++ "'"
        codeLines = 
            [
                makeLine indent ["let (", stateStr', ", top) = pop ", stateStr],
                makeLine indent ["newState <- case top of"],
                makeLine (indent + 1) ["Just v -> return $ push ", stateStr, " (doQueryType' v)"],
                makeLine (indent + 1) ["Nothing -> throwError (\"Operator (queryType) error. One operand needed; none provided!\") ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)

generateOpCode "printLine" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        stateStr' = stateStr ++ "'"
        codeLines = 
            [
                makeLine indent ["let (", stateStr', ", top) = pop ", stateStr],
                makeLine indent ["newState <- case top of"],
                makeLine (indent + 1) ["Just (String{chrs = cs, len = l}) -> putStrLn cs >> return ", stateStr],
                makeLine (indent + 1) ["Just v -> let vType = chrs $ doQueryType' v \
                \in throwError (\"Operator (printLine) error. Top of stack needs to be type String! Attempted type: \" ++ vType) ", stateStr],
                makeLine (indent + 1) ["Nothing -> throwError (\"Operator (printLine) error. Can't print from empty stack!\") ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)
generateOpCode "readLine" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines = 
            [
                makeLine indent ["newState <- ", "(getLine >>= (\\input -> return $ push ", stateStr, " String{chrs = input, len = length input}", "))"],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)
generateOpCode "printChar" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        stateStr' = stateStr ++ "'"
        codeLines = 
            [
                makeLine indent ["let (", stateStr', ", top) = pop ", stateStr],
                makeLine indent ["newState <- case top of"],
                makeLine (indent + 1) ["Just (Char c) -> putChar c >> return ", stateStr],
                makeLine (indent + 1) ["Just v -> let vType = chrs $ doQueryType' v \
                \in throwError (\"Operator (printChar) error. Top of stack needs to be type Char! Attempted type: \" ++ vType) ", stateStr],
                makeLine (indent + 1) ["Nothing -> throwError (\"Operator (printChar) error. Can't print Char from empty stack!\") ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)
generateOpCode "readChar" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines = 
            [
                makeLine indent ["newState <- ", "(getChar >>= (\\input -> return $ push ", stateStr, " (Char input)", "))"],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)
generateOpCode "print" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        stateStr' = stateStr ++ "'"
        codeLines = 
            [
                makeLine indent ["let (", stateStr', ", top) = pop ", stateStr],
                makeLine indent ["newState <- case top of"],
                makeLine (indent + 1) ["Just (String{chrs = cs, len = l}) -> putStr cs >> return ", stateStr],
                makeLine (indent + 1) ["Just v -> let vType = chrs $ doQueryType' v \
                \in throwError (\"Operator (print) error. Top of stack needs to be a String to be printed! Attempted type: \" ++ vType) ", stateStr],
                makeLine (indent + 1) ["Nothing -> throwError (\"Operator (print) error. One operand needed for print; none provided!\") ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)
generateOpCode "read" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines =
            [
                makeLine indent ["let doRead = (\\acc -> do ; isEnd <- isEOF ; \
                \if isEnd \
                    \then return acc \
                    \else do input <- getLine ; if (null input) then return acc else do doRead (acc ++ input ++ \"\\n\")", ")"],
                makeLine indent ["captured <- doRead \"\" "],
                makeLine indent ["let state", show $ stateCount + 1, " = push ", stateStr, " String{chrs = captured, len = length captured}"]
            ]
    in (codeLines, stateCount + 1)

generateOpCode "printError" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        stateStr' = stateStr ++ "'"
        codeLines =
            [   
                makeLine indent ["let (", stateStr', ", top) = pop ", stateStr],
                makeLine indent ["newState <- case top of"],
                makeLine (indent + 1) ["Just (String{chrs = err, len = _}) -> throwError err ", stateStr],
                makeLine (indent + 1) ["Just v -> let vType = chrs $ doQueryType' v \
                \in throwError (\"Operator (printError) error. String needed on top of stack for error to print! Attempted type: \" ++ vType) ", stateStr],
                makeLine (indent + 1) ["Nothing -> throwError (\"Operator (printError) error. One operand required; none provided!\") ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = ", stateStr]
            ]
    in (codeLines, stateCount + 1)

generateOpCode "debugPrintStack" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        codeLines =
            [   
                makeLine indent ["putStrLn \"----------------------------------------------\\nDEBUG START\""],
                makeLine indent ["putStrLn \"STACK START\""],
                makeLine indent ["printStack (reverse $ stack ", stateStr, ")"],
                makeLine indent ["putStrLn \"STACK END\""],
                makeLine indent ["putStrLn (\"STACK LENGTH: \" ++ (show $ length $ stack ", stateStr, "))"],
                makeLine indent ["putStrLn \"DEBUG END\\n----------------------------------------------\""]
            ]
    in (codeLines, stateCount)

generateOpCode "addField" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        stateStr' = stateStr ++ "'"
        codeLines =
            [   
                makeLine indent ["let (", stateStr', ", obj, fieldName, fieldVal) = pop3 ", stateStr],
                makeLine indent ["newState <- case (obj, fieldName, fieldVal) of "],
                makeLine (indent + 1) ["( Just (Object{fields = fs}), Just (String{chrs = name, len = _}), Just v ) -> \
                \ case (M.lookup name fs) of ; \
                \Just _ -> throwError (\"Operator (addField) error. Field \" ++ name ++ \" already exists in given object!\") ", stateStr, " ; ",
                "Nothing -> return $ push ", stateStr', " Object{fields = M.insert name v fs}"],
                makeLine (indent + 1) ["(Just v1, Just v2, Just v3) -> \
                \let (v1Type, v2Type, v3Type) = (chrs $ doQueryType' v1, chrs $ doQueryType' v2, chrs $ doQueryType' v3) \
                \in throwError (\"Operator (addField) error. Operands need to be Object String Value! \
                \Attempted types: \" ++ v1Type ++ \", \" ++ v2Type ++ \", and \" ++ v3Type) ", stateStr'],
                makeLine (indent + 1) ["(Nothing, Just v2, Just v3) -> throwError (\"Operator (addField) error. Three operands needed; only two provided!\") ", stateStr'],
                makeLine (indent + 1) ["(Nothing, Nothing, Just v3) -> throwError (\"Operator (addField) error. Three operands needed; only one provided!\") ", stateStr'],
                makeLine (indent + 1) ["(Nothing, Nothing, Nothing) -> throwError (\"Operator (addField) error. Three operands needed; none provided!\") ", stateStr'],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)
generateOpCode "removeField" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        stateStr' = stateStr ++ "'"
        codeLines =
            [   
                makeLine indent ["let (", stateStr', ", obj, removalKey) = pop2 ", stateStr],
                makeLine indent ["newState <- case (obj, removalKey) of"],
                makeLine (indent + 1) ["(Just (Object{fields = fs}), Just (String{chrs = name, len = l})) -> case (M.lookup name fs) of ; \
                \Just _ -> return $ push ", stateStr', " Object{fields = M.delete name fs} ; \
                \Nothing -> throwError (\"Operator (removeField) error. Field \" ++ name ++ \" doesn't exist in given object!\") ", stateStr'],
                makeLine (indent + 1) ["(Just v1, Just v2) -> let (v1Type, v2Type) = findTypeStrsForError v1 v2 \
                \in throwError (\"Operator (removeField) error. Operands need to be type Object and String! Attempted types: \" ++ v1Type ++ \" and \" ++ v2Type) ", stateStr'],
                makeLine (indent + 1) ["(Nothing, Just v2) -> throwError (\"Operator (removeField) error. Two operands needed; only one provided!\") ", stateStr'],
                makeLine (indent + 1) ["(Nothing, Nothing) -> throwError (\"Operator (removeField) error. Two operands needed; none provided!\") ", stateStr'],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)

generateOpCode "getField" indent stateCount =
    let stateStr = "state" ++ (show stateCount)
        stateStr' = stateStr ++ "'"
        codeLines =
            [   
                makeLine indent ["let (", stateStr', ", obj, findKey) = pop2 ", stateStr],
                makeLine indent ["newState <- case (obj, findKey) of"],
                makeLine (indent + 1) ["(Just (Object{fields = fs}), Just (String{chrs = name, len = l})) -> \
                \case (M.lookup name fs) of ; Just v -> return $ push (push ", stateStr', " Object{fields = fs} ", ") v", 
                " ; Nothing -> throwError (\"Operator (getField) error. Field \" ++ name ++ \" doesn't exist in given object!\") ", stateStr],
                makeLine (indent + 1) ["(Just v1, Just v2) -> let (v1Type, v2Type) = findTypeStrsForError v1 v2 \
                \in throwError (\"Operator (getField) error. Operands need to be type Object and String. Attempted types: \" ++ v1Type ++ \" and \" ++ v2Type) ", stateStr],
                makeLine (indent + 1) ["(Nothing, Just v2) -> throwError (\"Operator (getField) error. Two operands needed; only one provided!\") ", stateStr],
                makeLine (indent + 1) ["(Nothing, Nothing) -> throwError (\"Operator (getField) error. Two operands needed; none provided!\") ", stateStr],
                makeLine indent ["let state", show $ stateCount + 1, " = newState"]
            ]
    in (codeLines, stateCount + 1)  

generateOpCode op indent stateCount = ([makeLine indent ["throwError \"Unrecognized operator: ", op, "\" state", show stateCount]], stateCount)

generateCodeString' :: AstNode -> [String] -> Int -> Int -> ([String], Int)
generateCodeString' (Terminal (Word op)) lineAcc indent stateCount =
    let (codeStr, stateCount') = generateOpCode op indent stateCount
    in (lineAcc ++ codeStr, stateCount')
generateCodeString' (Terminal (Val v)) lineAcc indent stateCount = 
    let vType = chrs $ doQueryType' v
        pushVal = case vType of
                        "List" -> "List { items = M.empty, len = 0}"
                        "Object" -> "Object {fields = M.empty}"
                        "String" -> "String{chrs = " ++ (show $ chrs v) ++ ", len = " ++ (show $ len v) ++ "}"
                        _ -> show v 
        lineAcc' = lineAcc ++ [(nFourSpaces indent) ++ "let state" ++ (show $ stateCount + 1) ++ " = push state" ++ (show $ stateCount) ++ " (" ++ pushVal ++ ")"]
    in (lineAcc', stateCount + 1)
generateCodeString' (Expression []) lineAcc indent stateCount = (lineAcc, stateCount)
generateCodeString' (Expression (first:rest)) lineAcc indent stateCount = 
    let (lineAcc', stateCount') = generateCodeString' first lineAcc indent stateCount
    in generateCodeString' (Expression (rest)) lineAcc' indent stateCount'

--Takes in AST and returns the final string of the Haskell code generated from it. 
generateCodeString :: AstNode -> String
generateCodeString ast =
    let linesInit = 
            [
                "import Data.List",
                "import Data.Char",
                "import Data.Maybe",
                "import Debug.Trace",
                "import Text.Read (readMaybe)",
                "import System.IO",
                "import System.Environment",
                "import qualified Data.Map.Strict as M",
                "import Control.DeepSeq",
                "import Control.Exception",
                "import Data.Typeable",
                "import System.IO.Error (tryIOError)",
                "data Value = ",
                ((nFourSpaces 2) ++ "BigInteger Integer"),
                ((nFourSpaces 1) ++ "|" ++ (nFourSpaces 1) ++  "Integer Int"),
                ((nFourSpaces 1) ++ "|" ++ (nFourSpaces 1) ++ "Float Float"),
                ((nFourSpaces 1) ++ "|" ++ (nFourSpaces 1) ++ "Double Double"),
                ((nFourSpaces 1) ++ "|" ++ (nFourSpaces 1) ++ "String {chrs :: [Char], len :: Int}"),
                ((nFourSpaces 1) ++ "|" ++ (nFourSpaces 1) ++ "Char Char"),
                ((nFourSpaces 1) ++ "|" ++ (nFourSpaces 1) ++ "Boolean Bool"),
                ((nFourSpaces 1) ++ "|" ++ (nFourSpaces 1) ++ "List {items :: M.Map Int Value, len :: Int}"),
                ((nFourSpaces 1) ++ "|" ++ (nFourSpaces 1) ++ "Object {fields :: M.Map String Value}"),
                ((nFourSpaces 1) ++ "|" ++ (nFourSpaces 1) ++ "Box Int"),
                ((nFourSpaces 1) ++ "deriving(Eq, Show, Ord)"),
                "data Heap = Heap {",
                (nFourSpaces 1) ++ "freeList :: M.Map Int (),",
                (nFourSpaces 1) ++ "h :: M.Map Int Value,",
                (nFourSpaces 1) ++ "heapSize :: Int",
                "}",
                "data EDState = EDState {",
                (nFourSpaces 1) ++ "stack :: [Value]",
                "}",
                "data GeneralException = GeneralException String deriving (Show, Typeable)",
                "instance Exception GeneralException",
                "",
                "throwError :: String -> EDState -> IO EDState",
                "throwError msg = throw $ GeneralException msg",
                "",
                "doQueryType' :: Value -> Value",
                "doQueryType' (BigInteger _) = String{chrs = \"BigInteger\", len = length \"BigInteger\"}",
                "doQueryType' (Integer _) = String{chrs = \"Integer\", len = length \"Integer\"}",
                "doQueryType' (Float _) = String{chrs = \"Float\", len = length \"Float\"}",
                "doQueryType' (Double _) = String{chrs = \"Double\", len = length \"Double\"}",
                "doQueryType' String{chrs = _, len = _} = String{chrs = \"String\", len = length \"String\"}",
                "doQueryType' (Char _) = String{chrs = \"Char\", len = length \"Char\"}",
                "doQueryType' (Boolean _) = String{chrs = \"Boolean\", len = length \"Boolean\"}",
                "doQueryType' (List {items = _, len = _}) = String{chrs = \"List\", len = length \"List\"}",
                "doQueryType' (Object {fields = _}) = String{chrs = \"Object\", len = length \"Object\"}",
                "doQueryType' (Box _) = String{chrs = \"Box\", len = length \"Box\"}",
                "findTypeStrsForError :: Value -> Value -> (String, String)",
                "findTypeStrsForError x y = (chrs $ doQueryType' x, chrs $ doQueryType' y)",

                "pop :: EDState -> (EDState, Maybe Value)",
                "pop EDState{stack = []} = (EDState{stack = []}, Nothing)",
                "pop state = (EDState{stack = tail $ stack state}, Just $ head $ stack state)",
                "",

                "pop2 :: EDState -> (EDState, Maybe Value, Maybe Value)",
                "pop2 state = ",
                makeLine 1 ["let (state', top) = pop state"],
                makeLine 2 ["(state'', secondToTop) = pop state'"],
                makeLine 1 ["in (state'', secondToTop, top)"],
                "",

                "pop3 :: EDState -> (EDState, Maybe Value, Maybe Value, Maybe Value)",
                "pop3 state = ",
                makeLine 1 ["let (state', top) = pop state"],
                makeLine 2 ["(state'', secondToTop) = pop state'"],
                makeLine 2 ["(state''', thirdToTop) = pop state''"],
                makeLine 1 ["in (state''', thirdToTop, secondToTop, top)"],

                "push :: EDState -> Value -> EDState",
                "push EDState{stack = xs} v = EDState{stack = v:xs}",

                "printStack :: [Value] -> IO ()",
                "printStack [] = return ()",
                "printStack ((List {items = is, len = l}):xs) =",
                makeLine 1 ["putStrLn (\"[\" ++ (printList List {items = is, len = l} \"\" 0 True) ++ (if (l > 16) then \", ...]\" else \"]\")) >> printStack xs"],
                "printStack ((String {chrs = cs, len = l}):xs) =",

                makeLine 1 ["let pr = if l < 256 then cs else (init $ take 255 cs) ++ \"...\""],
                makeLine 1 ["in putStrLn (show (String {chrs = pr, len = l})) >> printStack xs"],
                "printStack ((Object{fields = fs}):xs) = putStrLn (\"{\" ++ (printObj (M.toList fs) \"\") ++ \"}\") >> printStack xs",
                "printStack ((Box (-1)):xs) = putStrLn \"Box NULL\" >> printStack xs",
                "printStack (x:xs) = print x >> printStack xs",

                "printList :: Value -> String -> Int -> Bool -> String",
                "printList List {items = is, len = l} acc index isLimited",
                makeLine 1 ["| (index < l) && (index < 16 || isLimited == False) ="],
                makeLine 2 ["let curr = case M.lookup index is of"],
                makeLine 4 ["Just i -> i"],
                makeLine 4 ["Nothing -> error \"SHOULD NEVER GET HERE!!!\""],
                makeLine 3 ["acc' = case curr of"],
                makeLine 4 ["List {items = ls, len = listLength} -> acc ++ (if (accSmall acc)"],
                makeLine 5 ["then \", [\""],
                makeLine 5 ["else \"[\") ++ (printList (List{items = ls, len = listLength}) \"\" 0 isLimited) ++ (if (isLimited && listLength > 16) then \", ...]\" else \"]\")"],
                makeLine 4 ["Object {fields = fs} -> acc ++ (if (accSmall acc) then \", {\" else \"{\") ++ (printObj (M.toList fs) \"\") ++ \"}\""],                
                makeLine 4 ["Box (-1) -> acc ++ (if (index > 0) then \", \" else \"\") ++ \"Box NULL\""],
                makeLine 4 ["i -> acc ++ (if (index > 0) then \", \" else \"\") ++ (show i)"],
                makeLine 2 ["in printList (List{items = is, len = l}) acc' (index + 1) isLimited"],
                makeLine 1 ["| otherwise = acc"],

                "printObj :: [(String, Value)] -> String -> String",
                "printObj [] acc = acc",
                "printObj ((name, val):xs) acc =",
                makeLine 1 ["let insStr = case val of"],
                makeLine 3 ["Object{fields = fs} -> \"{\" ++ (printObj (M.toList fs) \"\") ++ \"}\""],
                makeLine 3 ["List{items = is, len = l} -> \"[\" ++ (printList (List{items = is, len = l}) \"\" 0 True) ++ (if (l > 16) then \", ...]\" else \"]\")"],
                makeLine 3 ["String{chrs = cs, len = l} ->"],
                makeLine 4 ["let cs' = if l < 256 then cs else (init $ take 255 cs) ++ \"...\""],
                makeLine 4 ["in show $ String{chrs = cs', len = l}"],
                makeLine 3 ["Box (-1) -> \"Box NULL\""],
                makeLine 3 ["i -> show i"],
                makeLine 1 ["in printObj xs (acc ++ (if accSmall acc then \", \" else \"\") ++ name ++ \" : \" ++ insStr)"],

                "accSmall :: String -> Bool",
                "accSmall \"\" = False",
                "accSmall _ = True",

                "validIntToChar :: Int -> Bool",
                "validIntToChar num = (num >= (ord minBound)) && (num <= (ord maxBound))",

                "addVals :: Value -> Value -> Either Value String",
                "addVals (BigInteger a) (BigInteger b) = Left $ BigInteger (a + b)",
                "addVals (Integer a) (Integer b) = Left $ Integer (a + b)",
                "addVals (Double a) (Double b) = Left $ Double (a + b)",
                "addVals (Float a) (Float b) = Left $ Float (a + b)",
                "addVals (Boolean a) (Boolean b) = Left $ Boolean (a || b)",
                "addVals a b = ",
                makeLine 1 ["let (aType, bType) = findTypeStrsForError a b"],
                makeLine 1 ["in Right (\"Operator (+) error. Can't add types together that are not both types of BigIntegers, Integers, Floats, Doubles, or Booleans! \""],
                makeLine 2 ["++ \"Attempted types were: \""],
                makeLine 2 ["++ aType ++ \" and \" ++ bType)"],

                "subVals :: Value -> Value -> Either Value String",
                "subVals (BigInteger a) (BigInteger b) = Left $ BigInteger (b - a)",
                "subVals (Integer a) (Integer b) = Left $ Integer (b - a)",
                "subVals (Double a) (Double b) = Left $ Double (b - a)",
                "subVals (Float a) (Float b) = Left $ Float (b - a)",
                "subVals a b = ",
                makeLine 1 ["let (bType, aType) = findTypeStrsForError b a"],
                makeLine 1 ["in Right (\"Operator (-) error. Can't subtract types that are not both types of BigIntegers, Integers, Floats, or Doubles! \""],
                makeLine 2 ["++ \"Attempted types were: \""],
                makeLine 2 ["++ bType ++ \" and \" ++ aType)"],

                "multVals :: Value -> Value -> Either Value String",
                "multVals (BigInteger a) (BigInteger b) = Left $ BigInteger (a * b)",
                "multVals (Integer a) (Integer b) = Left $ Integer (a * b)",
                "multVals (Double a) (Double b) = Left $ Double (a * b)",
                "multVals (Float a) (Float b) = Left $ Float (a * b)",
                "multVals (Boolean a) (Boolean b) = Left $ Boolean (a && b)",
                "multVals a b =",
                makeLine 1 ["let (aType, bType) = findTypeStrsForError a b"],
                makeLine 1 ["in Right (\"Operator (*) error. Can't multiply types that are not both types of BigIntegers, Integers, Floats, Doubles, or Booleans! \""],
                makeLine 2 ["++ \"Attempted types were: \""],
                makeLine 2 ["++ aType ++ \" and \" ++ bType)"],

                "divideVals :: Value -> Value -> Either Value String",
                "divideVals (BigInteger a) (BigInteger b) = if (a /= 0) then Left $ BigInteger (b `div` a) else Right \"Operator (/) error. Can't divide by zero for type BigInteger!\"",
                "divideVals (Integer a) (Integer b) = if (a /= 0) then Left $ Integer (b `div` a) else Right \"Operator (/) error. Can't divide by zero for type Integer!\"",
                "divideVals (Double a) (Double b) = if (a /= 0.0) then Left $ Double (b / a) else Right \"Operator (/) error. Can't divide by zero for type Double!\"",
                "divideVals (Float a) (Float b) = if (a /= 0.0) then Left $ Float (b / a) else Right \"Operator (/) error. Can't divide by zero for type Float!\"",
                "divideVals a b =",
                makeLine 1 ["let (bType, aType) = findTypeStrsForError b a"],
                makeLine 1 ["in Right (\"Operator (/) error. Can't divide types that are not both types of BigIntegers, Integers, Floats, or Doubles! \""],
                makeLine 2 ["++ \"Attempted types were: \""],
                makeLine 2 ["++ bType ++ \" and \" ++ aType)"],

                "doEqual :: Value -> Value -> Either Value String",
                "doEqual (BigInteger a) (BigInteger b) = Left $ Boolean (a == b)",
                "doEqual (Integer a) (Integer b) = Left $ Boolean (a == b)",
                "doEqual (Float a) (Float b) = Left $ Boolean (a == b)",
                "doEqual (Double a) (Double b) = Left $ Boolean (a == b)",
                "doEqual (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Left $ Boolean ((al == bl) && (acs == bcs))",
                "doEqual (Char a) (Char b) = Left $ Boolean (a == b)",
                "doEqual (Boolean a) (Boolean b) = Left $ Boolean (a == b)",
                "doEqual (List {items = as, len = al}) (List {items = bs, len = bl}) = Left $ Boolean ((al == bl) && (as == bs))",
                "doEqual (Box bnA) (Box bnB) = Left $ Boolean $ bnA == bnB",
                "doEqual a b = ",
                makeLine 1 ["let (aType, bType) = findTypeStrsForError a b"],
                makeLine 1 ["in Right (\"Operator (==) error. Can't compare types that are not both types of \""],
                makeLine 2 ["++ \"BigIntegers, Integers, Floats, Doubles, Strings, Chars, Booleans, Lists, or Boxes! \""],
                makeLine 2 ["++ \"Attempted types were: \""],
                makeLine 2 ["++ aType ++ \" and \" ++ bType)"],       

                "doNotEqual :: Value -> Value -> Either Value String",
                "doNotEqual (BigInteger a) (BigInteger b) = Left $ Boolean (a /= b)",
                "doNotEqual (Integer a) (Integer b) = Left $ Boolean (a /= b)",
                "doNotEqual (Float a) (Float b) = Left $ Boolean (a /= b)",
                "doNotEqual (Double a) (Double b) = Left $ Boolean (a /= b)",
                "doNotEqual (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Left $ Boolean (acs /= bcs)",
                "doNotEqual (Char a) (Char b) = Left $ Boolean (a /= b)",
                "doNotEqual (Boolean a) (Boolean b) = Left $ Boolean (a /= b)",
                "doNotEqual (List {items = as, len = al}) (List {items = bs, len = bl}) = Left $ Boolean (as /= bs)",
                "doNotEqual (Box bnA) (Box bnB) = Left $ Boolean $ bnA /= bnB",
                "doNotEqual a b = ",
                makeLine 1 ["let (aType, bType) = findTypeStrsForError a b"],
                makeLine 1 ["in Right (\"Operator (/=) error. Can't compare types that are not both types of \""],
                makeLine 2 ["++ \"BigIntegers, Integers, Floats, Doubles, Strings, Chars, Booleans, Lists, or Boxes! \""],
                makeLine 2 ["++ \"Attempted types were: \""],
                makeLine 2 ["++ aType ++ \" and \" ++ bType)"], 

                "doGreaterThan :: Value -> Value -> Either Value String",
                "doGreaterThan (BigInteger a) (BigInteger b) = Left $ Boolean (a > b)",
                "doGreaterThan (Integer a) (Integer b) = Left $ Boolean (a > b)",
                "doGreaterThan (Float a) (Float b) = Left $ Boolean (a > b)",
                "doGreaterThan (Double a) (Double b) = Left $ Boolean (a > b)",
                "doGreaterThan (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Left $ Boolean (acs > bcs)",
                "doGreaterThan (Char a) (Char b) = Left $ Boolean (a > b)",
                "doGreaterThan (Boolean a) (Boolean b) = Left $ Boolean (a > b)",
                "doGreaterThan (List {items = as, len = al}) (List {items = bs, len = bl}) = Left $ Boolean (as > bs)",
                "doGreaterThan a b = ",
                makeLine 1 ["let (aType, bType) = findTypeStrsForError a b"],
                makeLine 1 ["in Right (\"Operator (>) error. Can't compare types that are not both types of \""],
                makeLine 2 ["++ \"BigIntegers, Integers, Floats, Doubles, Strings, Chars, Booleans, or Lists! \""],
                makeLine 2 ["++ \"Attempted types were: \""],
                makeLine 2 ["++ aType ++ \" and \" ++ bType)"],

                "doLessThan :: Value -> Value -> Either Value String",
                "doLessThan (BigInteger a) (BigInteger b) = Left $ Boolean (a < b)",
                "doLessThan (Integer a) (Integer b) = Left $ Boolean (a < b)",
                "doLessThan (Float a) (Float b) = Left $ Boolean (a < b)",
                "doLessThan (Double a) (Double b) = Left $ Boolean (a < b)",
                "doLessThan (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Left $ Boolean (acs < bcs)",
                "doLessThan (Char a) (Char b) = Left $ Boolean (a < b)",
                "doLessThan (Boolean a) (Boolean b) = Left $ Boolean (a < b)",
                "doLessThan (List {items = as, len = al}) (List {items = bs, len = bl}) = Left $ Boolean (as < bs)",
                "doLessThan a b = ",
                makeLine 1 ["let (aType, bType) = findTypeStrsForError a b"],
                makeLine 1 ["in Right (\"Operator (<) error. Can't compare types that are not both types of \""],
                makeLine 2 ["++ \"BigIntegers, Integers, Floats, Doubles, Strings, Chars, Booleans, or Lists! \""],
                makeLine 2 ["++ \"Attempted types were: \""],
                makeLine 2 ["++ aType ++ \" and \" ++ bType)"],

                "doGreaterThanEqualTo :: Value -> Value -> Either Value String",
                "doGreaterThanEqualTo (BigInteger a) (BigInteger b) = Left $ Boolean (a >= b)",
                "doGreaterThanEqualTo (Integer a) (Integer b) = Left $ Boolean (a >= b)",
                "doGreaterThanEqualTo (Float a) (Float b) = Left $ Boolean (a >= b)",
                "doGreaterThanEqualTo (Double a) (Double b) = Left $ Boolean (a >= b)",
                "doGreaterThanEqualTo (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Left $ Boolean (acs >= bcs)",
                "doGreaterThanEqualTo (Char a) (Char b) = Left $ Boolean (a >= b)",
                "doGreaterThanEqualTo (Boolean a) (Boolean b) = Left $ Boolean (a >= b)",
                "doGreaterThanEqualTo (List {items = as, len = al}) (List {items = bs, len = bl}) = Left $ Boolean (as >= bs)",
                "doGreaterThanEqualTo a b = ",
                makeLine 1 ["let (aType, bType) = findTypeStrsForError a b"],
                makeLine 1 ["in Right (\"Operator (>=) error. Can't compare types that are not both types of \""],
                makeLine 2 ["++ \"BigIntegers, Integers, Floats, Doubles, Strings, Chars, Booleans, or Lists! \""],
                makeLine 2 ["++ \"Attempted types were: \""],
                makeLine 2 ["++ aType ++ \" and \" ++ bType)"],

                "doLessThanEqualTo :: Value -> Value -> Either Value String",
                "doLessThanEqualTo (BigInteger a) (BigInteger b) = Left $ Boolean (a <= b)",
                "doLessThanEqualTo (Integer a) (Integer b) = Left $ Boolean (a <= b)",
                "doLessThanEqualTo (Float a) (Float b) = Left $ Boolean (a <= b)",
                "doLessThanEqualTo (Double a) (Double b) = Left $ Boolean (a <= b)",
                "doLessThanEqualTo (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Left $ Boolean (acs <= bcs)",
                "doLessThanEqualTo (Char a) (Char b) = Left $ Boolean (a <= b)",
                "doLessThanEqualTo (Boolean a) (Boolean b) = Left $ Boolean (a <= b)",
                "doLessThanEqualTo (List {items = as, len = al}) (List {items = bs, len = bl}) = Left $ Boolean (as <= bs)",
                "doLessThanEqualTo a b = ",
                makeLine 1 ["let (aType, bType) = findTypeStrsForError a b"],
                makeLine 1 ["in Right (\"Operator (<=) error. Can't compare types that are not both types of \""],
                makeLine 2 ["++ \"BigIntegers, Integers, Floats, Doubles, Strings, Chars, Booleans, or Lists! \""],
                makeLine 2 ["++ \"Attempted types were: \""],
                makeLine 2 ["++ aType ++ \" and \" ++ bType)"],

                "doModulo :: Value -> Value -> Either Value String",
                "doModulo (BigInteger a) (BigInteger b) = Left $ BigInteger (a `mod` b)",
                "doModulo (Integer a) (Integer b) = Left $ Integer (a `mod` b)",
                "doModulo a b =",
                makeLine 1 ["let (aType, bType) = findTypeStrsForError a b"],
                makeLine 1 ["in Right (\"Operator (%) error. Can't modulo types that are not both types of \""],
                makeLine 2 ["++ \"BigIntegers or Integers! \""],
                makeLine 2 ["++ \"Attempted types were: \""],
                makeLine 2 ["++ aType ++ \" and \" ++ bType)"],

                "doConcat :: Value -> Value -> Either Value String",
                "doConcat (String {chrs = acs, len = al}) (String {chrs = bcs, len = bl}) = Left $ String {chrs = acs ++ bcs, len = al + bl}",
                "doConcat List {items = as, len = al} List {items = bs, len = bl} =",
                makeLine 1 ["let List{items = cs, len = cl} = doConcat' List{items = as, len = al} List{items = M.empty, len = 0} 0 0"],
                makeLine 2 ["List{items = ds, len = dl} = doConcat' List{items = bs, len = bl} List{items = cs, len = cl} 0 al"],
                makeLine 1 ["in Left $ List{items = ds, len = dl}"],
                "doConcat a b =",
                makeLine 1 ["let (aType, bType) = findTypeStrsForError a b"],
                makeLine 1 ["in Right (\"Operator (++) error. Can't concatenate types that are not both types of List or String! \""],
                makeLine 2 ["++ \"Attempted types were: \""],
                makeLine 2 ["++ aType ++ \" and \" ++ bType)"],

                "doConcat' :: Value -> Value -> Int -> Int -> Value",
                "doConcat' List{items = is, len = l} List{items = accs, len = accLen} index offset",
                makeLine 1 ["|    (index < l) ="],
                makeLine 3 ["let ins = case (M.lookup index is) of"],
                makeLine 5 ["Just i -> i"],
                makeLine 5 ["Nothing -> error \"SHOULD NEVER GET HERE!!!\""],
                makeLine 4 ["accs' = M.insert (index + offset) ins accs"],
                makeLine 3 ["in doConcat' List{items = is, len = l} List{items = accs', len = accLen + 1} (index + 1) offset"],
                makeLine 1 ["|    otherwise = List{items = accs, len = accLen}"],

                "main :: IO EDState",
                "main = do",
                (nFourSpaces 1) ++ "let state0 = EDState{stack = []}"
            ]
        (newLines, stateCount) = generateCodeString' ast linesInit 1 0  
        linesFinal = newLines ++ [(nFourSpaces 1) ++ "(printStack $ reverse $ stack state" ++ (show stateCount) ++ ") >> return state" ++ (show stateCount)]
    in (intercalate "\n" linesFinal) ++ "\n"

main :: IO ()
main = do
    args <- getArgs

    let fileName = if (not $ null args) 
        then (args !! 0) 
        else error "Please provide an EcksDee file to compile!"

    putStrLn ("Opening and reading " ++ fileName)
    --Magic *snorts in Mr Bean*
    openResult <- tryIOError $ openFile fileName ReadMode
    fileStr <- case (openResult) of
            Left e -> error ("File Read Error. File " ++ fileName ++ " couldn't be opened because: " ++ (show e))
            Right handle -> (hGetContents handle) >>= (\str -> str `deepseq` ((hClose handle) >> (return str)))

    putStrLn ("Generating abstract syntax tree of " ++ fileName)
    let tokens = removeComments False [] (tokenize fileStr)
    let ast = parseExpression tokens
    putStrLn $ show $ ast --DELETE LATER!!!!

    let pgmStr = generateCodeString ast
    --This name replaces the .xd file extension with .hs so ghc can compile it.
    let haskellFileName = if (".xd" `isSuffixOf` fileName) then (init $ init $ init fileName) ++ ".hs" else fileName ++ ".hs"
    
    --Writes program string to haskellFileName and tries to compile it using ghc.
    putStrLn ("Writing to " ++ haskellFileName)
    haskellWriteRes <- tryIOError $ openFile haskellFileName WriteMode
    case haskellWriteRes of
        Left err -> error ("Unable to write to file " ++ haskellFileName ++ " because " ++ (show err))
        Right handle -> do
            hPutStr handle pgmStr 
            hClose handle
            putStrLn ("Compiling " ++ haskellFileName)
            res <- system ( "cat " ++ haskellFileName ++ " && " ++ "ghc " ++ haskellFileName)
            case res of 
                ExitSuccess -> do
                    putStrLn "Cleanup"
                    let baseName = init $ init $ init haskellFileName
                    cleanupRes <- system ("cat " ++ haskellFileName ++ " && " ++ "rm " ++ baseName ++ ".o" ++ " && " ++ "rm " ++ baseName ++ ".h*")
                    case cleanupRes of
                        ExitSuccess -> (putStrLn "Cleanup Successful") >> putStrLn ("Compilation complete!")
                        ExitFailure errMsg -> putStrLn ("Cleanup failed because " ++ (show errMsg))
                ExitFailure err -> putStrLn ("Compilation of " ++ haskellFileName ++ " failed because " ++ (show err))