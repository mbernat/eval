module Eval (Result, parse, exec) where

import Language.Haskell.Interpreter (InterpreterError, Interpreter, runInterpreter, setImports, eval, typeOf)

imports = ["Prelude"]
process :: String -> (String -> Interpreter String) -> IO (Either InterpreterError String)
process line f = runInterpreter $ setImports imports >> f line

data Cmd = Eval String | TypeOf String | Unknown
type Result = Maybe (Either String String)

parse :: String -> Cmd
parse (':' : 'e' : s) = Eval s
parse (':' : 't' : s) = TypeOf s
parse _ = Unknown

exec :: Cmd -> IO Result
exec (Eval s) = process s eval >>= post
exec (TypeOf s) = process s typeOf >>= post
exec Unknown = return Nothing

post :: Either InterpreterError String -> IO Result
post (Right s) = return $ Just $ Right s
post (Left e) = return $ Just $ Left $ show e
