import Eval

main = do
  line <- getLine
  let cmd = parse line
  exec cmd
