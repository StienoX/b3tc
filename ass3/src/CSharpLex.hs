module CSharpLex where

import Data.Char
import Control.Monad
import ParseLib.Abstract
import Prelude hiding ((<$>), (<$), (<*>), (<*), (*>), sequence)

data Token = POpen    | PClose      -- parentheses     ()
           | SOpen    | SClose      -- square brackets []
           | COpen    | CClose      -- curly braces    {}
           | Comma    | Semicolon
           | KeyIf    | KeyElse
           | KeyWhile | KeyReturn
           | KeyTry   | KeyCatch
           | KeyClass | KeyVoid
           | StdType   String       -- the 8 standard types
           | Operator  String       -- the 15 operators
           | UpperId   String       -- uppercase identifiers
           | LowerId   String       -- lowercase identifiers
           | ConstInt  Int
           | ConstBool Bool
           | ConstChar Char
           deriving (Eq, Show)

{-

type operatorPrecedence = [[Token]]
type operatorSeperator  = [Token]

opSep :: operatorSeperator
opSep = [POpen, PClose, Semicolon]

opOrd :: operatorPrecedence
opOrd = [
    [Operator "+", Operator "-"],
    [Operator "*", Operator "/"]
]

operatorSort :: [Token] -> operatorPrecedence -> [Token]
operatorSort a:opp:b:tokens opP@((x:xs):ys) | opp == x = 
-}

keyword :: String -> Parser Char String
keyword [] = succeed ""
keyword xs@(x:_) | isLetter x = do ys <- greedy (satisfy isAlphaNum)
                                   guard (xs == ys)
                                   return ys
                 | otherwise  = token xs


greedyChoice :: [Parser s a] -> Parser s a
greedyChoice = foldr (<<|>) empty


terminals :: [(Token, String)]
terminals =
    [ ( POpen     , "("      )
    , ( PClose    , ")"      )
    , ( SOpen     , "["      )
    , ( SClose    , "]"      )
    , ( COpen     , "{"      )
    , ( CClose    , "}"      )
    , ( Comma     , ","      )
    , ( Semicolon , ";"      )
    , ( KeyIf     , "if"     )
    , ( KeyElse   , "else"   )
    , ( KeyWhile  , "while"  )
    , ( KeyReturn , "return" )
    , ( KeyTry    , "try"    )
    , ( KeyCatch  , "catch"  )
    , ( KeyClass  , "class"  )
    , ( KeyVoid   , "void"   )
    ]


lexWhiteSpace :: Parser Char String
lexWhiteSpace = greedy (satisfy isSpace)

lexLowerId :: Parser Char Token
lexLowerId = (\x xs -> LowerId (x:xs)) <$> satisfy isLower <*> greedy (satisfy isAlphaNum)

lexUpperId :: Parser Char Token
lexUpperId = (\x xs -> UpperId (x:xs)) <$> satisfy isUpper <*> greedy (satisfy isAlphaNum)

lexConstInt :: Parser Char Token
lexConstInt = (ConstInt . read) <$> greedy1 (satisfy isDigit)

-- Char lexer, ex 1
lexConstChar :: Parser Char Token
lexConstChar = ConstChar <$ symbol '\'' <*> anySymbol <* symbol '\''
-- lexConstChar = ConstChar <$> symbol '\'' *> anySymbol <* symbol '\''

-- Bool lexer, ex 1
lexConstBool :: Parser Char Token
lexConstBool = ConstBool True <$ token "true" <|> ConstBool False <$ token "false"

lexEnum :: (String -> Token) -> [String] -> Parser Char Token
lexEnum f xs = f <$> choice (map keyword xs)

lexTerminal :: Parser Char Token
lexTerminal = choice [t <$ keyword s | (t,s) <- terminals]


stdTypes :: [String]
stdTypes = ["int", "long", "double", "float", "byte", "short", "bool", "char"]

operators :: [String]
operators = ["+", "-", "*", "/", "%", "&&", "||", "^", "<=", "<", ">=", ">", "==", "!=", "="]


lexToken :: Parser Char Token
lexToken = sSingleComment *> lexToken <|> greedyChoice -- Ex 3
             [ lexTerminal
             , lexEnum StdType stdTypes
             , lexEnum Operator operators
             , lexConstInt
             , lexConstChar -- Ex 1
             , lexConstBool -- Ex 1
             , lexLowerId
             , lexUpperId
             ]

lexicalScanner :: Parser Char [Token]
--lexicalScanner = lexWhiteSpace *> (choice [sSingleComment epsilon *> greedy (lexToken <* lexWhiteSpace) <* eof
lexicalScanner = lexWhiteSpace *> greedy (lexToken <* lexWhiteSpace) <* eof

-- Scans for single line tokens. Ex 3
sSingleComment :: Parser Char Char
sSingleComment = symbol '#' <* anySymbol <* token "\r\n"

sStdType :: Parser Token Token
sStdType = satisfy isStdType
    where isStdType (StdType _) = True
          isStdType _           = False

sUpperId :: Parser Token Token
sUpperId = satisfy isUpperId
    where isUpperId (UpperId _) = True
          isUpperId _           = False

sLowerId :: Parser Token Token
sLowerId = satisfy isLowerId
    where isLowerId (LowerId _) = True
          isLowerId _           = False

sConst :: Parser Token Token
sConst  = satisfy isConst
    where isConst (ConstInt  _) = True
          isConst (ConstChar _) = True -- Ex 1
          isConst (ConstBool _) = True
          isConst _             = False

sOperator :: Parser Token Token
sOperator = satisfy isOperator
    where isOperator (Operator _) = True
          isOperator _            = False


sSemi :: Parser Token Token
sSemi =  symbol Semicolon

