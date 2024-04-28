import System.IO
import System.Environment
import System.Exit (exitFailure, exitSuccess)
import ParSeeemcrd (myLexer, pProgram)
import Typing.TypeCheck (typeCheck)
import Interpreter.Eval (evaluate)

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
      putStrError $ "Parse error: " ++ show err
      exitFailure
    Right tree -> case typeCheck tree of
      Left err -> do
        putStrError $ "Type error: " ++ show err
        exitFailure
      Right _ -> do
        (result, _) <- evaluate tree
        case result of
          Left err -> do
            putStrError $ "Runtime error: " ++ show err
            exitFailure
          Right _ -> exitSuccess

putStrError :: String -> IO ()
putStrError = hPutStrLn stderr
