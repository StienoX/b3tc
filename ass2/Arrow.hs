{--
INFOB3TC – Assignment P2
Nick Swaerdens - 6977960
Stein Bout     - 6987729
--}

module Main where

import Prelude hiding ((<$>), (<$), (<*>), (<*), (*>), Left, Right)
import ParseLib.Abstract
import Data.Map (Map, foldrWithKey, fromList)
import qualified Data.Map as L
import Control.Monad (replicateM)
import Data.Char (isSpace)

import System.Directory
import Language
import Scanner
import Parser

import Debug.Trace


type Space    = Map Pos Contents
type Size     = Int
type Pos      = (Int, Int)
--data Contents = Empty | Lambda | Debris | Asteroid | Boundary deriving (Eq, Ord, Show)
type Contents = Pat

parseSpace :: Parser Char Space
parseSpace =
  do
    (mr,mc)  <-  parenthesised
                   ((,) <$> natural <* symbol ',' <*> natural) <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <-  replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
             zipWith (\ r cs  ->
             zipWith (\ c d   ->  ((r,c),d)) [0..] cs) [0..] css

spaces :: Parser Char String
spaces = greedy (satisfy isSpace)

contents :: Parser Char Contents
contents =
  choice (Prelude.map (\ (f,c) -> f <$ symbol c) contentsTable) <* spaces

contentsTable :: [(Contents,Char)]
contentsTable =
  [  (Empty,'.'),(Lambda,'\\'),(Debris,'%'),(Asteroid,'O'),(Boundary,'#')]

--type Ident = () defined in Language.hs
type Commands    = Cmds
data Heading     = R | L | U | D deriving (Show) --Right Left Up Down
type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step  =  Done  Space Pos Heading
           |  Ok    ArrowState
           |  Fail  String

-- Exercise 6
check :: Program -> Bool
check p = checkUndefined p
       && checkStartRule p
       && checkDupRules p
       && checkPatMatch p
  where
    checkUndefined _ = True
    checkStartRule _ = True
    checkDupRules  _ = True
    checkPatMatch  _ = True

-- Exercise 7
printSpace :: Space -> String
printSpace sp = show maxKeys ++ "\n" ++ printBoard sp
  where
    maxKeys = foldrWithKey (\(k, l) _ (k', l') ->  (max k k', max l l') ) (0, 0) sp
    (_, hSize) = maxKeys

    printBoard :: Map Pos Contents -> String
    printBoard = foldrWithKey (\k x ks -> (printEntry k $ lookup x contentsTable) ++ ks) ""
      where
        printEntry :: Pos -> Maybe Char -> String
        printEntry (_, x) (Just v)
          | x == hSize         = v : "\n"
          | otherwise          = v : ""
        printEntry _ Nothing   = ""

-- Execise 8

toEnvironment :: String -> Environment
toEnvironment   str  = (traceShow $ parseProgram $ alexScanTokens str) makeEnv $ check' $ parseProgram $ alexScanTokens str
  where check'  p = if check p then p else error ("Error in generating environment. Logic error in interpreting tokens.")
        makeEnv p = foldr (\(Rule ident cmds) env -> L.insert ident cmds env) L.empty p

-- Execise 9

step :: Environment -> ArrowState -> Step
step _   (ArrowState space pos heading [])        = Done space pos heading --Stack empty -> Done
step env (ArrowState space pos heading (s:stack)) = (traceShow $ stack) (traceShow $ heading) (traceShow $ pos) (traceShow $ s) exec s
  where
    exec Go         = Ok (ArrowState space                   (move space pos heading) heading          stack)
    exec Take       = Ok (ArrowState (updateSpace space pos) pos                      heading          stack)
    exec Mark       = Ok (ArrowState (mark space pos)        pos                      heading          stack)
    exec CNothing   = Ok (ArrowState space                   pos                      heading          stack)
    exec (Turn d)   = Ok (ArrowState space                   pos                      (turn d heading) stack)
    exec (Case d a) = handleCases d a
      where
        handleCases :: Dir -> Alts -> Step
        handleCases _ []                          = Fail "No matching case."
        handleCases _   ((Alt Underscore cmds):_) = Ok (ArrowState space pos heading (cmds ++ stack)) 
        handleCases dir ((Alt pat cmds):alts)     = case ele of
          Nothing | (pat == Boundary) -> Ok (ArrowState space pos heading (cmds ++ stack))
          Just x  | (x == pat)        -> Ok (ArrowState space pos heading (cmds ++ stack))
          _                           -> handleCases dir alts
          where
            ele :: Maybe Contents
            ele = L.lookup (peek pos (turn dir heading)) space

    exec (CIdent i) = case L.lookup i env of
      Nothing -> Fail "Rule doesn't exist"
      Just x  -> Ok (ArrowState space pos heading (x++stack))

mark :: Space -> Pos -> Space
mark space pos = L.insert pos Lambda space 

turn :: Dir -> Heading -> Heading
--Turn 90 to the Left
turn Left  R   = U 
turn Left  U   = L 
turn Left  D   = R
turn Left  L   = D

--Turn 90 to the right
turn Right L   = U
turn Right U   = R
turn Right D   = L
turn Right R   = D

--Turn forward lol
turn _ heading = heading

peek :: Pos -> Heading -> Pos
peek (y, x) U = (y - 1, x)
peek (y, x) R = (y, x + 1)
peek (y, x) D = (y + 1, x)
peek (y, x) L = (y, x - 1)

move :: Space -> Pos -> Heading -> Pos
move space p h = if checkAvaibleSpace space (peek p h) then (peek p h) else p

updateSpace :: Space -> Pos -> Space
updateSpace space pos = if checkAvaibleSpace space pos then L.insert pos Empty space else space

checkAvaibleSpace :: Space -> Pos -> Bool
checkAvaibleSpace space pos = case L.lookup pos space of
  Just Empty  -> True
  Just Lambda -> True
  Just Debris -> True
  _           -> False

getContent :: Space -> Pos -> Contents
getContent space pos = case L.lookup pos space of
  Just x  -> x
  Nothing -> Boundary

-- Execise 10
{-
When recursive calls are at the end of the command sequence the stack doesn't grow.
Because the other instructions are consumed when the next recursive call is called.
This does not happen when the recursive call is in front of other instructions.

For example take these two functions:
1. (Instruction) (Recursive Call)
2. (Recursive Call) (Instruction)

1. Has the recursion call on the tail/end
2. Has the recursion call on the front

When we do the recursion call once the stack looks the following:
1. (Instruction) (Recursive Call)
2. (Recursive Call) (Instruction) (Instruction)

When we continue this trend we can notice that the stack grows bigger when the recursion call is not on the end.
The deeper we are in the recusive call the bigger the stack becomes.
The practice of placing the recursion call on the end of the function and optimiziation that is gained is called tail recursion.

-}

-- Execise 11

interactive :: Environment -> ArrowState -> IO ()
interactive env as@(ArrowState space pos heading stack) = do
  putStrLn (printSpace space)
  stepped <- return $ step env as

  case stepped of
    Fail x     -> putStrLn x
    Done s _ _ -> putStrLn (printSpace s)
    Ok x       -> do
      putStrLn "Give input bitch."
      input <- getLine
      putStrLn ("This is your input: " ++ input ++ ", bye bitch.")

      case input of
        "quit"    -> putStrLn "Doei bitch."
        otherwise -> interactive env x

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch env as = case step env as of
  Fail _     -> (L.empty, (0,0), U)
  Done s p h -> (s, p, h)
  Ok x       -> batch env x

main = do
  putStrLn "Written by Stein Bout (6987729) and Nick Swaerdens (6977960)."
  
  env               <- getArrow
  space             <- getSpace
  putStrLn (printSpace space)
  pos               <- getPos
  heading           <- getHeading
  as                <- return $ ArrowState space pos heading (getStack env)

  putStrLn "Please select interactive or batch mode: "
  input <- getLine
  case input of
    "interactive" -> interactive env as
    "batch"       -> putStrLn $ (\(s, _, _) -> printSpace s) (batch env as)
    _             -> putStrLn "Error, invalid mode!"

getHeading :: IO Heading
getHeading = do
  putStrLn "Please provide starting Heading.\nPlease use one of the following options; u,d,r,l\n:"
  heading <- getLine 
  case heading of 
    "d" -> return D 
    "u" -> return U
    "l" -> return L
    "r" -> return R
    _   -> error "Not a valid direction"

parsePos :: Parser Char Pos
parsePos = (,) <$> (symbol '(' *> natural) <* symbol ',' <*> natural <* symbol ')'

getPos :: IO Pos
getPos = do
  putStrLn "Please provide the start position (y,x): "
  tuple <- getLine
  return $ fst $ head $ parse parsePos tuple

getSpace :: IO Space
getSpace = do 
  putStrLn "Please provide (relative) path to the .space file: "
  relativePathSpace <- return "/examples/AddInput2.space"--getLine
  absolutePathSpace <- (mappend getCurrentDirectory (pure relativePathSpace))
  fileContentsSpace <- readFile absolutePathSpace
  return $ fst $ head $ parse parseSpace fileContentsSpace

getArrow :: IO Environment
getArrow = do
  putStrLn "Please provide (relative) path to the .arrow file: "
  relativePathArrow <- return "/examples/Add.arrow"--getLine
  absolutePathArrow <- (mappend getCurrentDirectory (pure relativePathArrow))
  fileContentsArrow <- readFile absolutePathArrow
  return $ toEnvironment fileContentsArrow

getStack :: Environment -> Stack
getStack env = case L.lookup "start" env of
  Just x  -> x
  Nothing -> []
