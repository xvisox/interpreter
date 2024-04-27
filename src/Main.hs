import System.IO
import System.Environment
import System.Exit (exitFailure)
import ParSeeemcrd (myLexer, pProgram)
import Typing.TypeCheck (typeCheck)

main :: IO()
main = do
  args <- System.Environment.getArgs
  case args of
    [file] -> do
      content <- readFile file
      runInterpreter content
    [] -> putStrLn "No path provided, reading from stdin" >> getContents >>= runInterpreter
    _ -> putStrLn "Too many arguments provided"

runInterpreter :: String -> IO ()
runInterpreter code = do
  let tokens = myLexer code
  case pProgram tokens of
    Left err -> do
      putStrLn $ "Parse error: " ++ show err
      exitFailure
    Right tree -> case typeCheck tree of
      Left err -> do
        putStrLn $ "Type error: " ++ show err
        exitFailure
      Right _ -> putStrLn "Type check successful" -- TODO: Interpreter here
