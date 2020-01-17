module CSharpGram where

import ParseLib.Abstract hiding (braced, bracketed, parenthesised, (<$>), (<$), (<*>), (<*), (*>), sequence)
import CSharpLex

import Debug.Trace

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
           <|> parenthesised pExpr1

pExpr :: Parser Token Expr
pExpr = chainr pExprSimple (ExprOper <$> sOperator)
lol = (,) <$> sOperator <*> pExprSimple

-- chainr :: Parser s a -> Parser s (a -> a -> a) -> Parser s a 

pExpr0 = undefined

pExpr1 :: Parser Token Expr
pExpr1 = (\x y -> foldl (insertToken) x y) <$> pExprSimple <*> greedy ((,) <$> sOperator <*> pExprSimple)

--pExpr =  . (,) <$> Operator "+" <*> pExprSimple

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
testExpr = ExprOper (Operator "=") (ExprConst (ConstChar 'v')) (
            ExprOper (Operator "*") (ExprConst (ConstInt 5)) (
              ExprOper (Operator "+") (ExprConst (ConstInt 5)) (
                ExprOper (Operator "/") (ExprConst (ConstInt 9)) ( 
                  ExprOper (Operator "+") (ExprConst (ConstInt 5)) (ExprConst (ConstInt 7))))))

-- a = b = 1
testExpr2 = ExprOper (Operator "=") (ExprConst (ConstChar 'a')) (
              ExprOper (Operator "=") (ExprConst (ConstChar 'b')) (ExprConst (ConstInt 1)))

-- a + b + c
testExpr3 = ExprOper (Operator "+") (ExprConst (ConstChar 'a')) (
              ExprOper (Operator "+") (ExprConst (ConstChar 'b')) (ExprConst (ConstChar 'c')))

--mergeExpr :: Expr -> Token -> Expr -> Expr
--mergeExpr expr1 t expr2 = $ ExprOper t (sortExpr expr1) (sortExpr expr2)

sortExpr :: Expr -> Expr
sortExpr (ExprOper op lh@(ExprOper _ _ _) rh@(ExprOper _ _ _)) = jantje (ExprOper op (sortExpr lh) (sortExpr rh))
sortExpr (ExprOper op lh                  rh@(ExprOper _ _ _)) = jantje (ExprOper op lh (sortExpr rh))
sortExpr (ExprOper op lh@(ExprOper _ _ _) rh)                  = jantje (ExprOper op (sortExpr lh) rh)
sortExpr expr                                                  = expr


printExpr :: Expr -> String
printExpr (ExprOper (Operator x) lh@(ExprOper _ _ _) rh@(ExprOper _ _ _)) = (printExpr lh) ++ x ++ (printExpr rh)
printExpr (ExprOper (Operator x) lh                  rh@(ExprOper _ _ _)) = (showExpr  lh) ++ x ++ (printExpr rh)
printExpr (ExprOper (Operator x) lh@(ExprOper _ _ _) rh)                  = (printExpr lh) ++ x ++ (showExpr  rh)
printExpr (ExprOper (Operator x) lh rh)                                   = "(" ++ (showExpr lh) ++ x ++ (showExpr rh) ++ ")"

showExpr :: Expr -> String
showExpr (ExprConst (ConstInt x))  = show x
showExpr (ExprConst (ConstChar x)) = [x]
showExpr x                         = show x


  -- data Expr = ExprConst  Token
  -- | ExprVar    Token
  -- | ExprOper   Token Expr Expr
  -- deriving Show


-- 5*3+2*4 => (5*3)+(2*4)
--      +
--   *     *
-- 5   3 2   4
-- 4+3*8*1 => 4+((3*8)*1)

insertToken :: Expr -> (Token, Expr) -> Expr
insertToken expr@(ExprOper prev_op lexpr rexpr) (op,v) | prev_op `smaller` op = ExprOper op expr v
                                                       | otherwise            = ExprOper prev_op lexpr (insertToken rexpr (op,v))
insertToken expr (op,v)                                = ExprOper op expr v

tinsertToken :: Expr -> (Token, Expr) -> Expr
tinsertToken expr@(ExprOper prev_op lexpr rexpr) (op,v) | prev_op `smaller` op = trace ("1:" ++ printExpr (ExprOper op expr v)) (ExprOper op expr v)
                                                       | otherwise            = ExprOper prev_op lexpr (insertToken rexpr (op,v))
tinsertToken expr (op,v)                                = trace ("2:" ++ printExpr (ExprOper op expr v)) (ExprOper op expr v)

jantje :: Expr -> Expr
jantje expr@(ExprOper op lh (ExprOper op' lh' rh')) = 
  case compPrecedence op op' of
    True -> let newExpr = ExprOper op lh lh'
            in  ExprOper op' newExpr rh'
    _    -> expr
jantje expr = expr

data Assoc = LR | RL deriving (Eq, Show)

-- List of operators, their presedences and whether they're associated to the left or right. [(Operator, Presedence, Assoc)]
opAssoc :: [(Token, Int, Assoc)]
opAssoc = [
    (Operator "*",  1, LR),
    (Operator "/",  1, LR),
    (Operator "%",  1, LR),
    (Operator "+",  2, LR),
    (Operator "-",  2, LR),
    (Operator "<",  3, LR),
    (Operator "<=", 3, LR),
    (Operator ">",  3, LR),
    (Operator ">=", 3, LR),
    (Operator "==", 4, LR),
    (Operator "!=", 4, LR),
    (Operator "^",  5, LR),
    (Operator "&&", 6, LR),
    (Operator "||", 7, LR),
    (Operator "=",  9, RL)
  ]

smaller :: Token -> Token -> Bool
smaller = compPrecedence

-- Compares the precedence of two operators. 
-- Returns True when the first operator has a higher presendence than the second, otherwise False.
compPrecedence :: Token -> Token -> Bool
compPrecedence op1 op2 = let (prec1, asoc1) = getPrec op1 opAssoc
                             (prec2, asoc2) = getPrec op2 opAssoc
                         in prec1 < prec2 || (op1 == op2 && asoc1 == LR)
  where
    getPrec :: Token -> [(Token, Int, Assoc)] -> (Int, Assoc)
    getPrec _ []  = (8, LR)
    getPrec op@(Operator o) (((Operator x), i, a):xs)
      | o == x    = (i, a)
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

