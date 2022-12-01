module Main where
import Control.Monad.State
import Lexer
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)




main :: IO ()
main = do 
    args <- getArgs
    let filename = head args
    program <- TIO.readFile filename
    let eitherTokens = Lexer.getTokens program
    case eitherTokens of 
        (Left text) -> TIO.putStrLn text
        (Right tokens) -> print tokens

