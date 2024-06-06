--Jesse A. Jones
--Version: Alpha 0.2.2
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

    let pgmStr = "main = putStrLn \"Hello, World!!!\"" --This will need to be more than a template!
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
            res <- system ("ghc " ++ haskellFileName)
            case res of 
                ExitSuccess -> return ()
                ExitFailure err -> putStrLn (show err)