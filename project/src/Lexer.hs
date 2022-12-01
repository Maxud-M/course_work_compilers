{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}


module Lexer (getTokens, Tag, Token, Position, Fragment) where


import qualified Data.Text as T(Text, pack, unpack, length, index, take, drop)
import Data.Char(isLower)
import Control.Monad.State(State, evalState, get, put)
import Data.List() 
import Control.Monad (unless, when)
import Control.Monad.Except(ExceptT, runExceptT, throwError)
import Data.Either()



data Tag = LEFT_BRACKET | RIGHT_BRACKET | ARROW | TERM | COMMA | BACKSLASH | EOF deriving (Eq)
data Token = Token {frag:: !Fragment, tag:: !Tag, image:: !T.Text}
data Position = Position {index:: !Int, line:: !Int, column:: !Int}
data Fragment = Fragment {starting:: !Position, following:: !Position}
data LexerState = LexerState {program:: !T.Text, pos:: !Position}



instance Show Position where 
    show (Position _ l c) = "(" ++ show l ++ ", " ++ show c ++ ")"
    


instance Show Fragment where
    show (Fragment begin end) = show begin ++ "-" ++ show end

instance Show Token where 
    show token = T.unpack (image token) 

slice :: Int -> Int -> T.Text -> T.Text
slice start end = T.take (end-start) . T.drop start

whiteSpace :: [Char]
whiteSpace = ['\t', '\n', ' ', '\r']

type EvalMonad = ExceptT T.Text (State LexerState)

getTokens::T.Text -> Either T.Text [Token]
getTokens prog = evalState (runExceptT getAllTokens) LexerState {
    program = prog,
    pos = Position {
        index = 0, 
        line = 1, 
        column = 1
    }
}  





getAllTokens::EvalMonad [Token]
getAllTokens = do 
    token <- getToken
    if tag token == EOF 
        then return [token]
            else do
                tokens <- getAllTokens
                return $ token : tokens  





getTagFromChar::Char -> EvalMonad Tag
getTagFromChar '(' = return LEFT_BRACKET
getTagFromChar ')' = return RIGHT_BRACKET
getTagFromChar '-' = return ARROW
getTagFromChar ',' = return COMMA
getTagFromChar '\\' = return BACKSLASH
getTagFromChar char = do
                    if isLower char 
                        then return TERM
                        else throwError $ T.pack "expected TERM"

getToken:: EvalMonad Token
getToken = do
    skipWhiteSpaces
    eof <- isEof
    if not eof 
        then do
            s <- curCharacter
            tag <- getTagFromChar s
            curState <- get
            let begin = pos curState
            next
	    when (tag == ARROW)
	        $ do
	            s <- curCharacter
	  	    if s == '>'
		        then next
		        else throwError $ T.pack "expected ARROW"
            newState <- get
            let end = pos newState
            return Token {image = slice (index begin) (index end) (program newState), tag = tag, frag = Fragment{starting = begin, following = end}}
         else do
            curState <- get
            let end = pos curState
            return Token {image = "EOF", tag = EOF, frag = Fragment{starting = end, following = end}}




next::EvalMonad ()
next = do 
    curChar <- curCharacter
    st <- get
    put st {
        pos = (pos st) {
            index = index (pos st) + 1 
        }   
    }
    newState <- get
    if curChar == '\n'
        then put newState {
            pos = (pos newState) {
                line = line (pos newState) + 1,
                column = 1
            }
        }
        else put newState {
            pos = (pos newState) {
                column = column (pos newState) + 1
            }
        }
    


curCharacter::EvalMonad Char
curCharacter = do
    st <- get
    return $ program st `T.index` index (pos st)



isEof::EvalMonad Bool
isEof = do
    st <- get
    return $ index (pos st) >= T.length (program st)

skipWhiteSpaces:: EvalMonad ()
skipWhiteSpaces = do
    eof <- isEof
    unless eof $ do
        curChar <- curCharacter
        when (curChar `elem` whiteSpace) $ do 
            next
            skipWhiteSpaces
