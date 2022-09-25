module Parser (file) where

import Data.List (elemIndex)
import Expr (Expr (..))
import Relude
import Text.Parsec (ParsecT, alphaNum, letter, oneOf, parserFail, (<?>))
import Text.Parsec.Expr (Assoc (AssocLeft, AssocRight), Operator (Infix), OperatorTable, buildExpressionParser)
import qualified Text.Parsec.Token as P

type NamingContext = [Text]
type Parser = ParsecT Text () (Reader NamingContext)

file :: Parser Expr
file = whiteSpace *> expr

expr :: Parser Expr
expr = buildExpressionParser table term <?> "expression"

term :: Parser Expr
term =
    parens expr
        <|> typeTerm
        <|> lamTerm
        <|> piTerm
        <|> letTerm
        <|> varTerm
        <?> "simple expression"

typeTerm :: Parser Expr
typeTerm = Type <$ reserved "Type"

letTerm :: Parser Expr
letTerm = do
    i <- reserved "let" *> identifier <* symbol "≡"
    Let i <$> expr <* symbol "." <*> local (i :) expr

varTerm :: Parser Expr
varTerm = do
    ctx <- ask
    n <- identifier
    case elemIndex n ctx of
        Nothing -> parserFail $ "Variable not in scope: " <> toString n
        Just i -> pure $ Var n i

lamTerm :: Parser Expr
lamTerm = do
    i <- reserved "λ" *> binding <* symbol ":"
    Lam i <$> expr <* symbol "." <*> maybe id (local . (:)) i expr

piTerm :: Parser Expr
piTerm = do
    i <- reserved "Π" *> binding <* symbol ":"
    Pi i <$> expr <* symbol "." <*> maybe id (local . (:)) i expr

table :: OperatorTable Text () (Reader NamingContext) Expr
table =
    [ [binary "" App AssocLeft]
    , [binary "→" (Pi Nothing) AssocRight]
    ]

binary :: String -> (a -> a -> a) -> Assoc -> Operator Text () (Reader NamingContext) a
binary name fun = Infix $ reservedOp name $> fun

lexer :: P.GenTokenParser Text u (Reader NamingContext)
lexer = P.makeTokenParser language

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

identifier :: Parser Text
identifier = toText <$> P.identifier lexer

binding :: Parser (Maybe Text)
binding = Nothing <$ symbol "_" <|> Just <$> identifier

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

symbol :: String -> Parser String
symbol = P.symbol lexer

language :: P.GenLanguageDef Text st (Reader NamingContext)
language =
    P.LanguageDef
        { P.commentStart = "{-"
        , P.commentEnd = "-}"
        , P.commentLine = "--"
        , P.nestedComments = True
        , P.identStart = letter
        , P.identLetter = alphaNum <|> oneOf "_'"
        , P.opStart = P.opLetter language
        , P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , P.reservedOpNames = ["→", ".", "≡"]
        , P.reservedNames = ["Type", "λ", "Π", "let"]
        , P.caseSensitive = True
        }
