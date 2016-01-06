module Lexer where

import Text.Parsec.String
import Text.Parsec.Language
import Text.ParserCombinators.Parsec hiding (spaces)

import qualified Text.Parsec.Token as Token

ops :: [String]
ops = words "-> + - * \\ = < <= > >= ;"

keyWords :: [String]
keyWords = words "fst snd true false fun if then else let in printInt inL inR case of Int Bool Unit ()"

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser (emptyDef {
             Token.identStart = letter
            , Token.identLetter = alphaNum <|> char '_'
            , Token.reservedNames = keyWords
            , Token.reservedOpNames = ops
            , Token.commentStart = "(*"
            , Token.commentEnd = "*)"
        })

-- Tokens
lexeme :: String -> Parser ()
lexeme s = string s >> whitespace

integer :: Parser Integer
integer = Token.integer lexer           -- parses an integer

identifier :: Parser String
identifier = Token.identifier lexer     -- parses a valid identifier in diML

reserved :: String -> Parser ()
reserved = Token.reserved lexer         -- parses a reserved word like "if"

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer     -- parses a reserved operation like "<="

parens :: Parser a -> Parser a
parens = Token.parens lexer             -- parses parens surrounding the parser passed to it

whitespace :: Parser ()
whitespace = Token.whiteSpace lexer     -- parses whitespace (most used token!)

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer         -- parses comma (needed for let exprs)

semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep lexer           -- semicolons (probably not needed)

