module CSharpGram where

import ParseLib.Abstract hiding (braced, bracketed, parenthesised, (<$>), (<$), (<*>), (<*), (*>), sequence)
import CSharpLex


data Class = Class Token [Member]
    deriving Show

data Member = MemberD Decl
            | MemberM Type Token [Decl] Stat
            deriving Show

data Stat = StatDecl   Decl
          | StatExpr   Expr
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatReturn Expr
          | StatBlock  [Stat]
          deriving Show

data Expr = ExprConst  Token
          | ExprVar    Token
          | ExprOper   Token Expr Expr
          deriving Show

data Decl = Decl Type Token
    deriving Show

data Type = TypeVoid
          | TypePrim  Token
          | TypeObj   Token
          | TypeArray Type
          deriving (Eq,Show)


parenthesised p = pack (symbol POpen) p (symbol PClose)
bracketed     p = pack (symbol SOpen) p (symbol SClose)
braced        p = pack (symbol COpen) p (symbol CClose)

pExprSimple :: Parser Token Expr
pExprSimple =  ExprConst <$> sConst
           <|> ExprVar   <$> sLowerId
           <|> parenthesised pExpr

pExpr :: Parser Token Expr
pExpr = chainr pExprSimple (ExprOper <$> sOperator)

{-
5 * 5 + 9 / 5 % 7

Base:
[ConstInt 5, Operator "*", ConstInt 5, Operator "+", ConstInt 9, Operator "/", ConstInt 5, Operator "%", ConstInt 7]
1:
[ConstInt 25, Operator "+", ConstInt 9, Operator "/", ConstInt 5, Operator "%", ConstInt 7]
1:
[ConstInt 25, Operator "+", ConstInt 1.8, Operator "%", ConstInt 7]
1:
[ConstInt 25, Operator "+", ConstInt 1.8]
2:
[ConstInt 26.8]
-}

-- List of operators and their presedences. [(Operator, Presedence)]
opAssoc :: [(Token, Int)]
opAssoc = [
    (Operator "*",  1),
    (Operator "/",  1),
    (Operator "%",  1),
    (Operator "+",  2),
    (Operator "-",  2),
    (Operator "<",  3),
    (Operator "<=", 3),
    (Operator ">",  3),
    (Operator ">=", 3),
    (Operator "==", 4),
    (Operator "!=", 4),
    (Operator "^",  5),
    (Operator "&&", 6),
    (Operator "||", 7),
    (Operator "=",  8)
    ]

pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> pMeth

pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi

pStat :: Parser Token Stat
pStat =  StatExpr <$> pExpr <*  sSemi
     <|> StatIf     <$ symbol KeyIf     <*> parenthesised pExpr <*> pStat <*> optionalElse
     <|> StatWhile  <$ symbol KeyWhile  <*> parenthesised pExpr <*> pStat
     <|> StatReturn <$ symbol KeyReturn <*> pExpr               <*  sSemi
     <|> pBlock
     where optionalElse = option ((\_ x -> x) <$> symbol KeyElse <*> pStat) (StatBlock [])


pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)


pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
    where
        methRetType = pType <|> (const TypeVoid <$> symbol KeyVoid)
        methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

pType0 :: Parser Token Type
pType0 =  TypePrim <$> sStdType
      <|> TypeObj  <$> sUpperId

pType :: Parser Token Type
pType = foldr (const TypeArray) <$> pType0 <*> many (bracketed (succeed ()))


pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

pDeclSemi :: Parser Token Decl
pDeclSemi = const <$> pDecl <*> sSemi

pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)

