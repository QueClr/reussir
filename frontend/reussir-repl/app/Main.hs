module Main (main) where

import Control.Exception (SomeException, bracketOnError, catch, evaluate)
import System.Console.Haskeline
import System.Console.Haskeline.IO

data Op = Add | Sub | Mul | Div
    deriving (Show)

data Expr
    = Lit Double
    | Call Op Expr Expr
    deriving (Show)

tokenize :: String -> [String]
tokenize [] = []
tokenize (c : cs)
    | c `elem` " \t\n" = tokenize cs
    | c == '(' = "(" : tokenize cs
    | c == ')' = ")" : tokenize cs
    | otherwise =
        let (token, rest) = span (\x -> x `notElem` " \t\n()") (c : cs)
         in token : tokenize rest

parse :: String -> Expr
parse = fst . parseExpr . tokenize

parseOp :: String -> Op
parseOp "+" = Add
parseOp "-" = Sub
parseOp "*" = Mul
parseOp "/" = Div
parseOp op = error $ "unknown operator: " ++ op

parseExpr :: [String] -> (Expr, [String])
parseExpr ("(" : op : xs) =
    let (arg1, rest1) = parseExpr xs
        (arg2, rest2) = parseExpr rest1
     in case rest2 of
            ")" : rest3 -> (Call (parseOp op) arg1 arg2, rest3)
            _ -> error "expected closing paren"
parseExpr (n : xs) = (Lit (read n), xs)
parseExpr [] = error "unexpected EOF"

main :: IO ()
main =
    bracketOnError
        (initializeInput defaultSettings)
        cancelInput -- This will only be called if an exception such
        -- as a SigINT is received.
        (\hd -> loop hd >> closeInput hd)
  where
    loop :: InputState -> IO ()
    loop hd = do
        minput <- queryInput hd (getInputLine "λ> ")
        case minput of
            Nothing -> return ()
            Just "quit" -> return ()
            Just input -> do
                result <-
                    catch
                        ( do
                            let ast = parse input
                            astStr <- evaluate (show ast)
                            return $ Right astStr
                        )
                        ( \(e :: SomeException) ->
                            return $ Left $ "Parse error: " ++ show e
                        )
                case result of
                    Right astStr -> queryInput hd $ outputStrLn astStr
                    Left errMsg -> queryInput hd $ outputStrLn errMsg
                loop hd
