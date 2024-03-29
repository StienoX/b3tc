module CSharpCode where

import Prelude hiding (LT, GT, EQ, (<$>), (<$), (<*>), (<*), (*>), sequence)
import Data.Map as M
import Data.Char
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM

import Debug.Trace

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
fMembMeth t (LowerId x) ps s = let firstAddr = negate $ length ps + 1
                                   env       = fromList $ zip (fmap (\(Decl _ (LowerId x)) -> x) ps) [firstAddr .. ]
                               in  [LABEL x, LINK 0] ++ s env ++ [UNLINK, RET] -- Set the LABEL and LINK, pass the environment, and UNLICK, RET after method call.

fStatDecl :: Decl -> Env -> Code
fStatDecl _ _ = []

fStatExpr :: (ValueOrAddress -> Env -> Code) -> Env -> Code
fStatExpr e env = e Value env ++ [pop]

fStatIf :: (ValueOrAddress -> Env -> Code) -> (Env -> Code) -> (Env -> Code) -> Env -> Code
fStatIf e s1 s2 env = c ++ [BRF (n1 + 2)] ++ s1 env ++ [BRA n2] ++ s2 env
    where c        = e Value env
          (n1, n2) = (codeSize (s1 env), codeSize (s2 env))

fStatWhile :: (ValueOrAddress -> Env -> Code) -> (Env -> Code) -> Env -> Code
fStatWhile e s1 env = [BRA n] ++ s1 env ++ c ++ [BRT (-(n + k + 2))]
    where c      = e Value env
          (n, k) = (codeSize (s1 env), codeSize c)

fStatReturn :: (ValueOrAddress -> Env -> Code) -> Env -> Code
fStatReturn e env = e Value env ++ [STR RR] -- Store return result into the result register (RR).

fStatBlock :: [Env -> Code] -> Env -> Code
fStatBlock xs env = xs >>= ($ env)

fExprCon :: Token -> ValueOrAddress -> Env -> Code
fExprCon (ConstInt  n) _ _ = [LDC n]
fExprCon (ConstChar n) _ _ = [LDC $ ord n]
fExprCon (ConstBool n) _ _ = [LDC $ fromEnum n]

fExprVar :: Token -> ValueOrAddress -> Env -> Code
fExprVar (LowerId x) va env
  | M.null env = [LDC 0] -- [Temporary] Default value for variables initialized without value, prevents odd behaviour.
  | otherwise  = let loc = (traceShow env) (env ! x)
                 in case va of
                     Value   -> [LDL  loc]
                     Address -> [LDLA loc]

fExprOp :: Token -> (ValueOrAddress -> Env -> Code) -> (ValueOrAddress -> Env -> Code) -> ValueOrAddress -> Env -> Code
fExprOp (Operator "=")  e1 e2 _ env = (traceShow $ e2 Address env) (traceShow $ e1 Address env) e2 Value env ++ [LDS 0] ++ e1 Address env ++ [STA 0]
fExprOp (Operator "||") e1 e2 _ env = e1 Value env ++ [BRT $ codeSize $ e2 Value env ++ [AJS 1], AJS 1] ++ e2 Value env -- if e1 is true, skip past second arg. if e2 is false, take value of second arg as output of OR.
fExprOp (Operator "&&") e1 e2 _ env = e1 Value env ++ [BRF $ codeSize $ e2 Value env ++ [AJS 1], AJS 1] ++ e2 Value env -- if e1 is false, skip past second arg. if e2 is true, take value of second arg as output of AND.
fExprOp (Operator op)   e1 e2 _ env = e1 Value env ++ e2 Value env ++ [opCodes ! op]

fExprMethod :: Token -> [ValueOrAddress -> Env -> Code] -> ValueOrAddress -> Env -> Code
fExprMethod (LowerId "print") xs _ env = (xs >>= \x -> x Value env) ++ ([negate (length xs - 1) .. 0] >>= \x -> LDS x : [TRAP 0]) -- Copy each result on stack before calling TRAP since TRAP pops.
fExprMethod (LowerId s)       xs _ env = (xs >>= \x -> x Value env) ++ [Bsr s, AJS (negate $ length xs), LDR RR] -- Remove function parameters at the end of a function call and push the value of RR onto the stack.

opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]
