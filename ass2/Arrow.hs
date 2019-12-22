{--
INFOB3TC – Assignment P2
Nick Swaerdens - 6977960
Stein Bout     - 6987729
--}

module Main where

import Prelude hiding ((<*), (<$))
import ParseLib.Abstract
import Data.Map (Map, foldrWithKey, fromList)
import qualified Data.Map as L
import Control.Monad (replicateM)
import Data.Char (isSpace)

import Language
import Scanner
import Parser

type Space    = Map Pos Contents
type Size     = Int
type Pos      = (Int, Int)
data Contents = Empty | Lambda | Debris | Asteroid | Boundary deriving (Eq, Ord, Show)

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
type Heading     = R | L | U | D --Right Left Up Down
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
    (vSize, hSize) = maxKeys

    printBoard :: Map Pos Contents -> String
    printBoard = foldrWithKey (\k x ks -> (printEntry k $ lookup x contentsTable) ++ ks) ""
      where
        printEntry :: Pos -> Maybe Char -> String
        printEntry (_, x) (Just v)
          | x == hSize         = v : "\n"
          | otherwise          = v : ""
        printEntry _ Nothing   = ""
{--
    outputList :: [[Char]]
    outputList = map (\_ -> (map (\_ -> ' ') [0..hSize] )) [0..vSize] 

    placeAt :: Pos -> Maybe Char -> [[Char]] -> [[Char]]
    placeAt (a,b) (Just c) xs = undefined
    placeAt _ Nothing xs      = xs

    lookup' :: Pos -> Char
    lookup' p = case lookup p sp of
      | Just c = c
      | Nothing = 
        where
          printEntry :: Pos -> Maybe Char -> String
          printEntry (_, x) (Just v)
            | x == hSize         = v : "\n"
            | otherwise          = v : ""
          printEntry _ Nothing   = " "
--}    

-- Execise 8

toEnvironment :: String -> Environment
toEnvironment     = makeEnv . check' . parseProgram . alexScanTokens
  where check'  p = if check p then p else error ("Error in generating environment. Logic error in interpreting tokens.")
        makeEnv p = foldr (\(Rule ident cmds) env -> L.insert ident cmds env) L.empty p

-- Execise 9
{-
type Commands    = Cmds
type Heading     = Dir
type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step  =  Done  Space Pos Heading
           |  Ok    ArrowState
           |  Fail  String


type Space    = Map Pos Contents
data Contents = Empty | Lambda | Debris | Asteroid | Boundary

-}

step :: Environment -> ArrowState -> Step

step _ (ArrowState space pos heading []) = Done space pos heading --Stack empty -> Done
step env (ArrowState space pos heading (s:stack)) = exec s
  where 
    exec Go       = Ok (ArrowState space            (move space pos heading) heading          stack)
    exec Take     = Ok (ArrowState (take space pos) pos                      heading          stack)
    exec Mark     = Ok (ArrowState (mark space pos) pos                      heading          stack)
    exec CNothing = Ok (ArrowState space            pos                      heading          stack)
    exec Turn d   = Ok (ArrowState space            pos                      (turn d heading) stack)
    exec Case d a = 
    exec CIdent i =
  
rule :: Step -> Environment -> Step
rule 

handleCases :: Dir -> Heading ->
handleCases = undefined
  where

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

move :: Space -> Pos -> Heading -> Pos
move space (y,x) U = if checkAvaibleSpace (y+1,x  ) then (y+1,x  )
move space (y,x) R = if checkAvaibleSpace (y  ,x+1) then (y  ,x+1)
move space (y,x) L = if checkAvaibleSpace (y  ,x-1) then (y  ,x-1)
move space (y,x) D = if checkAvaibleSpace (y-1,x  ) then (y-1,x  )

take :: Space -> Pos -> Space
take space pos = if checkAvaibleSpace space pos then L.insert pos Empty space else space

checkAvaibleSpace :: Space -> Pos -> Bool
checkAvaibleSpace space pos = case lookup pos space of
  Just Empty  -> True
  Just Lambda -> True
  Just Debris -> True
  _           -> False

main :: IO ()
--main = putStrLn $ printSpace (fromList [((0,0), Empty), ((0,1), Asteroid), ((1,0), Debris), ((1,1), Debris), ((2,0), Debris), ((2,1), Debris), ((4,0), Debris), ((5,0), Debris), ((6,0), Debris), ((7,0), Debris), ((8,0), Debris), ((9,0), Debris), ((10,0), Debris), ((11,0), Debris), ((12,0), Debris), ((13,0), Debris), ((14,0), Debris), ((15,0), Debris), ((16,0), Debris), ((17,0), Debris), ((18,0), Debris), ((19,0), Debris), ((20,0), Debris), ((21,0), Debris), ((22,0), Debris), ((23,0), Debris), ((24,0), Debris), ((25,0), Debris), ((26,0), Debris), ((27,0), Debris), ((28,0), Debris)])
main = undefined