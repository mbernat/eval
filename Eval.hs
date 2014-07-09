module Eval (parse, exec) where

import Language.Haskell.Interpreter (InterpreterError, Interpreter, runInterpreter, setImports, eval, typeOf)

imports = ["Prelude"]
process :: String -> (String -> Interpreter String) -> IO (Either InterpreterError String)
process line f = runInterpreter $ setImports imports >> f line

data Cmd = Eval String | TypeOf String | Unknown

parse :: String -> Cmd
parse (':' : 'e' : s) = Eval s
parse (':' : 't' : s) = TypeOf s
parse _ = Unknown

exec :: Cmd -> IO ()
exec (Eval s) = process s eval >>= post
exec (TypeOf s) = process s typeOf >>= post
exec Unknown = putStrLn "Command not supported"

post (Right s) = putStrLn s
post (Left e) = putStrLn $ show e
