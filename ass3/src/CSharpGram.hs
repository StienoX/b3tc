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
Stap 1:
zet eerst als expr van eerste 2 waardes
bv: 2*5

Stap 2:
kijk naar huidige en check prio met volgende
bv: 2*5+3+4*5

3+
   2*5


als laagere prio zet hem er bove anders door gaan naar volgende in de boom. geen elementen meer over plaats onderaan. zo door
-}

-- x = 5 * (5 + (9 / (5 + 7)))

-- (x * (y + z)) -> ((x * y) + z)
-- base
-- (x * (y + z))
-- 1
-- let a = (x * y)
-- 2
-- in a + z

-- x = 5 * 5 + 9 / 5 + 7
testExpr  = ExprOper (Operator "*") (ExprConst (ConstInt 5)) (
              ExprOper (Operator "+") (ExprConst (ConstInt 5)) (
                ExprOper (Operator "/") (ExprConst (ConstInt 9)) ( 
                  ExprOper (Operator "+") (ExprConst (ConstInt 5)) (ExprConst (ConstInt 7)))))

sortExpr :: Expr -> Expr
sortExpr (ExprOper op lh@(ExprOper _ _ _) rh@(ExprOper _ _ _)) = jantje (ExprOper op (sortExpr lh) (sortExpr rh))
sortExpr (ExprOper op lh                  rh@(ExprOper _ _ _)) = jantje (ExprOper op lh (sortExpr rh))
sortExpr (ExprOper op lh@(ExprOper _ _ _) rh)                  = jantje (ExprOper op (sortExpr lh) rh)
sortExpr (ExprOper op lh rh)                                   = ExprOper op lh rh

printExpr :: Expr -> String
printExpr (ExprOper (Operator x) lh@(ExprOper _ _ _) rh@(ExprOper _ _ _)) = (printExpr lh) ++ x ++ (printExpr rh)
printExpr (ExprOper (Operator x) lh                  rh@(ExprOper _ _ _)) = (showExpr lh)  ++ x ++ (printExpr rh)
printExpr (ExprOper (Operator x) lh@(ExprOper _ _ _) rh)                  = (printExpr lh) ++ x ++ (showExpr rh)
printExpr (ExprOper (Operator x) lh rh)                                   = "(" ++ (showExpr lh)  ++ x ++ (showExpr rh)  ++ ")"

showExpr :: Expr -> String
showExpr (ExprConst (ConstInt x)) = show x
showExpr x                        = show x

type OperatorPrecedence = [[Token]]

opOrd :: OperatorPrecedence
opOrd = [
    [Operator "+", Operator "-"],
    [Operator "*", Operator "/"]
  ]


  -- data Expr = ExprConst  Token
  -- | ExprVar    Token
  -- | ExprOper   Token Expr Expr
  -- deriving Show

insertToken :: Expr -> Expr -> Expr -> Expr
insertToken expr operator value = undefined


jantje :: Expr -> Expr
jantje base@(ExprOper op lh (ExprOper op' lh' rh')) = 
  case compPrecedence op op' of
    True -> let newExpr = ExprOper op lh lh'
            in  ExprOper op' newExpr rh'
    _    -> base
jantje base = base

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

-- Compares the precedence of two operators. 
-- Returns True when the first operator has a higher presendence than the second, otherwise False.
compPrecedence :: Token -> Token -> Bool
compPrecedence op1 op2 = (getPrec op1 opAssoc) < (getPrec op2 opAssoc)
  where
    getPrec :: Token -> [(Token, Int)] -> Int
    getPrec _ []  = 8
    getPrec op@(Operator o) (((Operator x), i):xs)
      | o == x    = i
      | otherwise = getPrec op xs

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

