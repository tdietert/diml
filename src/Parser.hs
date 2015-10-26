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
contents p = whiteSpace *> p

-- parses semicolon terminated expression
topLevel :: Parser [DimlExpr]
topLevel = many1 $ do
   e <- expr <* reservedOp ";"
   return e 
 
-- parses, type checks, and evaluates file
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

-- infix apply expr (Add to op table as [apply] for curried look):
-- parses whitespace in between function __ arg for an apply expr
-- as long as whitespace not followed by another operator!
--apply :: Expr.Operator String () Data.Functor.Identity.Identity DimlExpr
--apply = Expr.Infix space Expr.AssocLeft
--    where space = Apply 
--                <$ whiteSpace
--                <* notFollowedBy (choice . map reservedOp $ ops)

opTable :: Expr.OperatorTable String () Data.Functor.Identity.Identity DimlExpr
opTable = [ [ binary "*" Expr.AssocLeft
            , binary "/" Expr.AssocLeft]
          , [ binary "+" Expr.AssocLeft
            , binary "-" Expr.AssocLeft ]
          , [ binary "<" Expr.AssocLeft
            , binary ">" Expr.AssocLeft 
            , binary "==" Expr.AssocLeft ] 
        ]      

expr :: Parser DimlExpr
expr = Expr.buildExpressionParser opTable factor

intExpr :: Parser DimlExpr
intExpr = Lit . DInt <$> integer

varExpr :: Parser DimlExpr
varExpr = Var <$> identifier

boolExpr :: Parser DimlExpr
boolExpr =  Lit DTrue <$ reserved "true"
        <|> Lit DFalse <$ reserved "false"

-- this could be cleaned up...
funExpr :: Parser DimlExpr
funExpr = Fun <$> try name <*> arg <*> body
    where name = reserved "fun" *> identifier 
          arg = char '(' *> identifier  -- one argument functions
--        argType = reservedOp ":" *> typeExpr
--        returnType = char ')' *> reservedOp ":" *> typeExpr
          body = char ')' *> whiteSpace *> reservedOp "=" *> expr

lamExpr :: Parser DimlExpr
lamExpr = Lam <$> try arg <*> body
    where arg  = reservedOp "\\" *> identifier
--        typ  = reservedOp ":" *> typeExpr
          body = reservedOp "->" *> expr 

applyExpr :: Parser DimlExpr
applyExpr = Apply <$> varExpr <*> parens expr

ifExpr :: Parser DimlExpr
ifExpr = If <$> e1 <*> e2 <*> e3
    where e1 = reserved "if" *> expr
          e2 = reserved "then" *> expr
          e3 = reserved "else" *> expr

-- explicitly a pair: (x,y)
tupleExpr :: Parser DimlExpr
tupleExpr = do 
    char '(' >> whiteSpace
    e1 <- expr
    char ',' >> whiteSpace
    e2 <- expr
    char ')' >> whiteSpace 
    return $ Tuple e1 e2

letExpr :: Parser DimlExpr
letExpr = do
    reserved "let"
    decls <- commaSep $ funExpr <|> declExpr
    reserved "in"
    body <- expr
    return $ Let decls body    

-- this parser parses a let declaration
-- ex: (x = 5) from 'let (x = 5) in x'
declExpr :: Parser DimlExpr
declExpr = do
    var <- try $ identifier <* reservedOp "="
    varAsgnmt <- expr
    return $ Decl var varAsgnmt

prIntExpr :: Parser DimlExpr
prIntExpr = do
    toPrint <- reserved "printInt" *> parens expr
    return $ PrintInt toPrint

-- Types: 
-- int | bool | arrow type type
-------------------------------
boolType :: Parser Type
boolType = tBool <$ reserved "Bool" 

intType :: Parser Type
intType = tInt <$ reserved "Int"

-- right associative type 
arrowType :: Parser Type
arrowType = tTypeExpr `chainr1` arrow
    where arrow = TArr <$ reservedOp "->" 

-- base type exprs
tTypeExpr :: Parser Type
tTypeExpr = boolType 
        <|> try (parens arrowType)
        <|> intType 

typeExpr :: Parser Type
typeExpr = try arrowType <|> tTypeExpr
-------------------------------

factor :: Parser DimlExpr
factor =  funExpr
      <|> lamExpr
      <|> try applyExpr
      <|> boolExpr
      <|> ifExpr
      <|> letExpr
      <|> intExpr
      <|> declExpr
      <|> varExpr
      <|> try tupleExpr
      <|> prIntExpr
      <|> parens expr