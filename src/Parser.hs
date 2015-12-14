module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language 
import qualified Text.Parsec.Expr as Expr

import Control.Applicative hiding (many, (<|>))
import Data.Functor.Identity
import System.IO

import Lexer 
import Syntax
import Type

--------------------------------------
-- Parsing Programs from IO
--------------------------------------

-- parses contents of string or file,
-- excluding leading whitespace
contents :: Parser a -> Parser a
contents p = whitespace *> p

-- parses semicolon terminated expression
topLevel :: Parser [DimlExpr]
topLevel = many1 $ do
   e <- expr <* reservedOp ";"
   return e 
 
-- parses file
parseFile :: String -> IO ()
parseFile filename = do
    program <- readFile filename
    print $ parseExpr program

-- parses expression without semicolon
parseExpr :: String -> Either ParseError DimlExpr
parseExpr str = parse (contents expr) "<stdin>" str

-- parses program
parseTopLevel :: String -> Either ParseError [DimlExpr]
parseTopLevel str = parse (contents topLevel) "<stdin>" str

--------------------------------------

-- binary and prefix args are written differently, but do the same thing!
binary :: Name -> Expr.Assoc -> (Expr.Operator String () Data.Functor.Identity.Identity DimlExpr)
binary name = Expr.Infix (reservedOp name >> return (BinOp name))

prefix :: Name -> (DimlExpr -> DimlExpr) -> (Expr.Operator String () Data.Functor.Identity.Identity DimlExpr)
prefix name label = Expr.Prefix (reservedOp name *> return (\x -> label x))

apply :: Expr.Operator String () Data.Functor.Identity.Identity DimlExpr
apply = Expr.Infix space Expr.AssocLeft
    where space = Apply 
                <$ whitespace
                <* notFollowedBy (choice . map reservedOp $ ops)

opTable :: Expr.OperatorTable String () Data.Functor.Identity.Identity DimlExpr
opTable = [ [apply, prefix "snd" (Builtins . TupFst), prefix "fst" (Builtins . TupSnd)]
          , [ binary "*" Expr.AssocLeft
            , binary "/" Expr.AssocLeft]
          , [ binary "+" Expr.AssocLeft
            , binary "-" Expr.AssocLeft ]
          , [ binary "<" Expr.AssocLeft
            , binary ">" Expr.AssocLeft 
            , binary "==" Expr.AssocLeft ] 
        ]      

-- add "annot" to all exprs as last field. see intExpr
annot :: Parser Type
annot = reservedOp ":" *> typeExpr

expr :: Parser DimlExpr
expr = Expr.buildExpressionParser opTable factor

intExpr :: Parser DimlExpr
intExpr = Lit . DInt <$> integer

varExpr :: Parser DimlExpr
varExpr = Var <$> identifier 

boolExpr :: Parser DimlExpr
boolExpr =  Lit DTrue <$ reserved "true" 
        <|> Lit DFalse <$ reserved "false"

funExpr :: Parser DimlExpr
funExpr = do
    try $ reserved "fun"
    name <- identifier
    char '(' >> whitespace
    arg <- identifier
    argTyp <- optionMaybe annot
    char ')' >> whitespace
    retTyp <- optionMaybe annot
    reservedOp "="
    body <- expr
    return $ Fun name arg argTyp retTyp body 

lamExpr :: Parser DimlExpr
lamExpr = Lam <$> try arg <*> optionMaybe annot <*> body
    where arg  = reservedOp "\\" *> identifier
          body = reservedOp "->" *> expr 

ifExpr :: Parser DimlExpr
ifExpr = If <$> try e1 <*> e2 <*> e3
    where e1 = reserved "if" *> expr
          e2 = reserved "then" *> expr
          e3 = reserved "else" *> expr

-- explicitly a pair: (x,y)
tupleExpr :: Parser DimlExpr
tupleExpr = do 
    e1 <- try $ reservedOp "(" *> expr <* reservedOp "," 
    e2 <- expr <* reservedOp ")" 
    ann <- optionMaybe annot
    return $ Tuple e1 e2 ann

letExpr :: Parser DimlExpr
letExpr = do
    try $ reserved "let"
    decls <- commaSep $ funExpr <|> declExpr
    reserved "in"
    body <- expr
    return $ transLet decls body
    where transLet :: [DimlExpr] -> DimlExpr -> DimlExpr
          transLet [] body = body
          transLet (d:decls) body = Let d (transLet decls body)

-- this parser parses a let declaration
-- ex: (x = 5) from 'let (x = 5) in x'
declExpr :: Parser DimlExpr
declExpr = do
    var <- try $ identifier <* reservedOp "="
    varAsgnmt <- expr
    return $ Decl var varAsgnmt 

prIntExpr :: Parser DimlExpr
prIntExpr = do
    toPrint <- try $ reserved "printInt" *> parens expr
    return $ PrintInt toPrint

parensExpr :: Parser DimlExpr
parensExpr = Parens <$> parens expr <*> optionMaybe annot

-- Types: 
-- int | bool | arrow type type | prod type type
-------------------------------
boolType :: Parser Type
boolType = tBool <$ reserved "Bool" 

intType :: Parser Type
intType = tInt <$ reserved "Int"

prodType :: Parser Type
prodType = do
    t1 <- char '(' *> typeExpr
    t2 <- char ',' *> typeExpr <* char ')'
    return $ TProd t1 t2

-- right associative type 
arrowType :: Parser Type
arrowType = tTypeExpr `chainr1` arrow
    where arrow = TArr <$ reservedOp "->" 

-- base type exprs
tTypeExpr :: Parser Type
tTypeExpr =  boolType 
         <|> prodType
         <|> intType 

typeExpr :: Parser Type
typeExpr =  try arrowType 
        <|> tTypeExpr 
        <|> parens tTypeExpr
-------------------------------

factor :: Parser DimlExpr
factor = declExpr
     <|> funExpr
     <|> lamExpr
     <|> boolExpr
     <|> ifExpr
     <|> letExpr
     <|> intExpr
     <|> prIntExpr
     <|> varExpr
     <|> tupleExpr
     <|> parensExpr
