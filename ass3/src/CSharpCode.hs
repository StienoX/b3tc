module CSharpCode where

import Prelude hiding (LT, GT, EQ, (<$>), (<$), (<*>), (<*), (*>), sequence)
import Data.Map as M
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM


data ValueOrAddress = Value | Address
    deriving Show

type Env = Map String Int

codeAlgebra :: CSharpAlgebra Code Code (Env -> Code) (ValueOrAddress -> Env -> Code)
codeAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatBlock)
    , (fExprCon, fExprVar, fExprOp, fExprMethod)
    )

fClas :: Token -> [Code] -> Code
fClas c ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> Code
fMembDecl _ = []

fMembMeth :: Type -> Token -> [Decl] -> (Env -> Code) -> Code
fMembMeth t (LowerId x) ps s = [LABEL x, LINK 0] ++ s env ++ [STR R3, UNLINK, RET]
  where env = let firstAddr = negate (length ps + 1)
              in  fromList $ zip (fmap (\(Decl _ (LowerId x)) -> x) ps) [firstAddr .. -2]

fStatDecl :: Decl -> Env -> Code
fStatDecl _ _ = []

fStatExpr :: (ValueOrAddress -> Env -> Code) -> Env -> Code
fStatExpr e env = e Value env ++ [pop]

fStatIf :: (ValueOrAddress -> Env -> Code) -> (Env -> Code) -> (Env -> Code) -> Env -> Code
fStatIf e s1 s2 env = c ++ [BRF (n1 + 2)] ++ s1 env ++ [BRA n2] ++ s2 env
    where
        c        = e Value env
        (n1, n2) = (codeSize (s1 env), codeSize (s2 env))

fStatWhile :: (ValueOrAddress -> Env -> Code) -> (Env -> Code) -> Env -> Code
fStatWhile e s1 env = [BRA n] ++ s1 env ++ c ++ [BRT (-(n + k + 2))]
    where
        c = e Value env
        (n, k) = (codeSize (s1 env), codeSize c)

fStatReturn :: (ValueOrAddress -> Env -> Code) -> Env -> Code
fStatReturn e env = e Value env ++ [pop] ++ [STR R3, RET] -- UNLINK?

fStatBlock :: [Env -> Code] -> Env -> Code
fStatBlock xs env = xs >>= ($ env)

fExprCon :: Token -> ValueOrAddress -> Env -> Code
fExprCon (ConstInt n) _ env = [LDC n]

fExprVar :: Token -> ValueOrAddress -> Env -> Code
fExprVar (LowerId x) va env = let loc = (env ! x) 
                              in case va of
                                    Value    -> [LDL  loc]
                                    Address  -> [LDLA loc]

fExprOp :: Token -> (ValueOrAddress -> Env -> Code) -> (ValueOrAddress -> Env -> Code) -> ValueOrAddress -> Env -> Code
fExprOp (Operator "=")  e1 e2 _ env = e2 Value env ++ [LDS 0] ++ e1 Address env ++ [STA 0]
fExprOp (Operator "||") e1 e2 _ env = e1 Value env ++ [BRT $ codeSize (e2 Value env)] ++ e2 Value env ++ [OR]
fExprOp (Operator "&&") e1 e2 _ env = e1 Value env ++ [BRF $ codeSize (e2 Value env)] ++ e2 Value env ++ [AND]
fExprOp (Operator op)   e1 e2 _ env = e1 Value env ++ e2 Value env ++ [opCodes ! op]

fExprMethod :: Token -> [ValueOrAddress -> Env -> Code] -> ValueOrAddress -> Env -> Code
fExprMethod (LowerId "print") xs _ env = (xs >>= (\x -> x Value env)) ++ ([negate ((length xs) + 1) .. 0] >>= (\x -> LDS x : [TRAP 0]))
fExprMethod (LowerId s)       xs _ env = (xs >>= (\x -> x Value env)) ++ [Bsr s, AJS (negate (length xs))]

opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]
