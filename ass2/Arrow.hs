{-
INFOB3TC – Assignment P2
Nick Swaerdens - 6977960
Stein Bout     - 6987729
-}

module Main where

import Prelude hiding ((<*), (<$))
import ParseLib.Abstract
import Data.Map (Map)
import qualified Data.Map as L
import Control.Monad (replicateM)
import Data.Char (isSpace)

import Language

type Space    = Map Pos Contents
type Size     = Int
type Pos      = (Int, Int)
data Contents = Empty | Lambda | Debris | Asteroid | Boundary

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

-- These three should be defined by you
--type Ident = () defined in Language.hs
type Commands = ()
type Heading = ()

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step  =  Done  Space Pos Heading
           |  Ok    ArrowState
           |  Fail  String

-- Exercise 6
check :: Program -> Bool
check p = checkUndefined 
       && checkStartRule
       && checkDupRules
       && checkPatMatch
  where
    checkUndefined _ = False
    checkStartRule _ = False
    checkDupRules  _ = False
    checkPatMatch  _ = False

-- Exercise 7
printSpace :: Space -> String
printSpace sp = printSize
  where
    printSpace size = '(' : (read size) : ',' : (read size) : ")\n"

main = undefined