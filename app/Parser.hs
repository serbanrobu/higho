module Parser (file) where

import Ast (Ast (..))
import Relude
import Text.Parsec (alphaNum, chainr1, eof, letter, oneOf, (<?>))
import Text.Parsec.Expr (Assoc (AssocLeft, AssocRight), Operator (Infix), OperatorTable, buildExpressionParser)
import Text.Parsec.Text (Parser)
import qualified Text.Parsec.Token as P

file :: Parser Ast
file = whiteSpace *> ast <* eof

ast :: Parser Ast
ast = buildExpressionParser table term <?> "expression"

term :: Parser Ast
term =
    parens ast
        <|> Type <$ reserved "Type"
        <|> abstr "λ" Lam
        <|> reserved "let" *> parens (Let <$> identifier <* symbol "≡" <*> ast)
            `chainr1` pure (.) <* symbol "." <*> ast
        <|> abstr "Π" Pi
        <|> Var <$> identifier
        <?> "simple expression"
  where
    abstr :: String -> Abs -> Parser Ast
    abstr s f = symbol s *> abstrHead f `chainr1` pure (.) <* symbol "." <*> ast

    abstrHead :: Abs -> Parser (Ast -> Ast)
    abstrHead f = parens $ f <$> maybeIdentifier <* symbol ":" <*> ast

table :: OperatorTable Text () Identity Ast
table =
    [ [binary "" App AssocLeft]
    , [binary "→" (Pi Nothing) AssocRight]
    ]

binary :: String -> (a -> a -> a) -> Assoc -> Operator Text () Identity a
binary name fun = Infix $ reservedOp name $> fun

type Abs = Maybe Text -> Ast -> Ast -> Ast

lexer :: P.GenTokenParser Text u Identity
lexer = P.makeTokenParser language

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

identifier :: Parser Text
identifier = toText <$> P.identifier lexer

maybeIdentifier :: Parser (Maybe Text)
maybeIdentifier = (Nothing <$ symbol "_") <|> (Just <$> identifier)

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

symbol :: String -> Parser String
symbol = P.symbol lexer

language :: P.GenLanguageDef Text st Identity
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
