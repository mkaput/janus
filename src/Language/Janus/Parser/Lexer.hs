module Language.Janus.Parser.Lexer (
  identifier,
  keyword,
  op,
  reservedOp,
  charLiteral,
  stringLiteral,
  natural,
  integer,
  float,
  naturalOrFloat,
  decimal,
  hexadecimal,
  octal,
  symbol,
  lexeme,
  whiteSpace,
  parens,
  braces,
  angles,
  brackets,
  semi,
  comma,
  colon,
  dot,
  semiSep,
  semiSep1,
  commaSep,
  commaSep1,
  boolean,
  literal
) where
import           Control.Applicative ((<|>))
import           Control.Monad
import           Language.Janus.AST
import           Text.Parsec         (alphaNum, letter, oneOf, (<?>))
import qualified Text.Parsec.Token   as T

janusDef :: T.LanguageDef st
janusDef = T.LanguageDef {
        T.commentStart    = "/*",
        T.commentEnd      = "*/",
        T.commentLine     = "//",
        T.nestedComments  = True,
        T.identStart      = letter,
        T.identLetter     = alphaNum <|> oneOf "_'",
        T.opStart         = T.opLetter janusDef,
        T.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~",
        T.reservedNames   = ["and", "as", "break", "case", "class",
                             "const", "continue", "do", "else", "enum",
                             "False", "fn", "for", "if", "in",
                             "let", "loop", "mod", "or", "return",
                             "trait", "True", "type", "while", "yield"],
        T.reservedOpNames = ["!", "~", "+", "-", "++", "--", "**", "*", "/", "<<", ">>",
                             "<", "<=", ">", ">=", "==", "!=", "&", "^", "|", "(" , ")",
                             "[", "]", "()"],
        T.caseSensitive   = True
    }

lexer = T.makeTokenParser janusDef

identifier      = T.identifier      lexer
keyword         = T.reserved        lexer
op              = T.operator        lexer
reservedOp      = T.reservedOp      lexer
charLiteral     = T.charLiteral     lexer
stringLiteral   = T.stringLiteral   lexer
natural         = T.natural         lexer
integer         = T.integer         lexer
float           = T.float           lexer
naturalOrFloat  = T.naturalOrFloat  lexer
decimal         = T.decimal         lexer
hexadecimal     = T.hexadecimal     lexer
octal           = T.octal           lexer
symbol          = T.symbol          lexer
lexeme          = T.lexeme          lexer
whiteSpace      = T.whiteSpace      lexer
parens          = T.parens          lexer
braces          = T.braces          lexer
angles          = T.angles          lexer
brackets        = T.brackets        lexer
semi            = T.semi            lexer
comma           = T.comma           lexer
colon           = T.colon           lexer
dot             = T.dot             lexer
semiSep         = T.semiSep         lexer
semiSep1        = T.semiSep1        lexer
commaSep        = T.commaSep        lexer
commaSep1       = T.commaSep1       lexer

boolean = keyword "True" *> return True
          <|> keyword "False" *> return False

literal = either JInt JDouble <$> naturalOrFloat
          <|> JInt <$> integer
          <|> JChar <$> charLiteral
          <|> JStr <$> stringLiteral
          <|> JBool <$> boolean
          <|> reservedOp "()" *> return JUnit
          <?> "literal"
