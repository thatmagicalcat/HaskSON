import Control.Monad
import Data.List
import System.Environment
import Data.Char

main = do
    args <- getArgs
    case args of 
        [filename] -> do
            contents <- readFile filename
            either putStrLn (putStrLn . prettyPrint) (parse contents)

        _ -> putStrLn "Usage: haskson <filename.json>"

data Ast
    = AstObject [(String, Ast)]
    | AstList [Ast]
    | AstString String
    | AstInt Int
    | AstFloat Float 
    deriving (Show)

data Token
    = TkStringLit String
    | TkIntLit Int
    | TkFloatLit Float
    | TkEOF
    | TkLParen
    | TkRParen
    | TkLBracket
    | TkRBracket
    | TkLCurly
    | TkRCurly
    | TkComma
    | TkColon
    deriving (Show)

prettyPrint :: Ast -> String
prettyPrint = go 0
    where
        ident n = intercalate "" $ take (n * 2) $ repeat " "

        go :: Int -> Ast -> String
        go _ (AstString s) = "String(" ++ show s ++ ")"
        go _ (AstInt i)    = "Int(" ++ show i ++ ")"
        go _ (AstFloat f)  = "Float(" ++ show f ++ ")"

        -- Array
        go identLevel (AstList xs) =
            "Array([\n"
            ++ (intercalate ",\n" $ map (ident (identLevel + 1) ++) (map (go (identLevel + 1)) xs))
            ++ "\n"
            ++ ident identLevel
            ++ "])"

        go identLevel (AstObject xs) =
            "Object({\n"
            ++ (intercalate ",\n"
                $ [ ident (identLevel + 1) ++ show key ++ ": " ++ go (identLevel + 1) value | (key, value) <- xs])
            ++ "\n"
            ++ ident identLevel
            ++ "}"

parse :: String -> Either String Ast
parse = tokenize >=> parseExpr >=> checkEOF 
    where
        checkEOF :: (Ast, [Token]) -> Either String Ast
        checkEOF (a, [TkEOF]) = Right a
        checkEOF (_, (t:_))   = Left $ "Expected EOF, got: " ++ show t

type Parser = Either String (Ast, [Token])

parseExpr :: [Token] -> Parser
parseExpr (t:rest) = case t of
    TkIntLit i    -> Right (AstInt i, rest)
    TkFloatLit i  -> Right (AstFloat i, rest)
    TkStringLit i -> Right (AstString i, rest)
    TkLBracket    -> parseArray rest
    TkLCurly      -> parseObj rest

parseArray :: [Token] -> Parser
parseArray tokens = go tokens []
    where
        go :: [Token] -> [Ast] -> Parser
        -- empty array
        go (TkRBracket : rest) acc = Right (AstList $ reverse acc, rest) 
        go ts acc = parseExpr ts >>= \(val, rest1) -> case rest1 of
            (TkComma    : rest2) -> go rest2 (val : acc)
            (TkRBracket : rest2) -> Right (AstList $ reverse (val:acc), rest2)
            _                    -> Left "Expected ',' or ']' in array"

parseObj :: [Token] -> Parser
parseObj tokens = go tokens []
    where
        go :: [Token] -> [(String, Ast)] -> Parser
        -- empty object
        go (TkRCurly : more) acc = Right (AstObject $ reverse acc, more)
        go (TkStringLit key : TkColon : rest) acc = parseExpr rest >>= \(val, rest') ->
            case rest' of
                TkComma  : more -> go more $ (key, val) : acc
                TkRCurly : more -> Right (AstObject $ reverse ((key, val) : acc), more)

tokenize :: String -> Either String [Token]
tokenize [] = Right [TkEOF]
tokenize (c:cs)
    | isSpace c = t cs
    | isDigit c =
        let (digits, rest) = span (\x -> isDigit x || x == '.') (c:cs)
        in if '.' `elem` digits
          then case reads digits :: [(Float, String)] of
                 [(val, "")] -> (TkFloatLit val :) <$> t rest
                 _           -> Left $ "Invalid float literal: " ++ digits
          else case reads digits :: [(Int, String)] of
                 [(val, "")] -> (TkIntLit val :) <$> (t rest)
                 _           -> Left $ "Invalid integer literal: " ++ digits
    | otherwise = case c of
        '(' -> (TkLParen:)   <$> t cs
        ')' -> (TkRParen:)   <$> t cs
        '[' -> (TkLBracket:) <$> t cs
        ']' -> (TkRBracket:) <$> t cs
        '{' -> (TkLCurly:)   <$> t cs
        '}' -> (TkRCurly:)   <$> t cs
        ',' -> (TkComma:)    <$> t cs
        ':' -> (TkColon:)    <$> t cs
        '"' -> let (str, rest) = span (/= '"') cs in
                   if null rest
                      then Left "Unterminated string literal"
                      else fmap (TkStringLit str :) (t $ drop 1 rest)
        _   -> Left $ "Unknown character: '" ++ [c] ++ "'"
    where t = tokenize
