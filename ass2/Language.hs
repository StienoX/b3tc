module Language where

import Prelude hiding (Left, Right, Nothing)

type Program = [Rule]
type Cmds    = [Cmd]
type Alts    = [Alt]

data Rule    = Rule Ident Cmds
data Cmd     = Go | Take | Mark | Nothing | Turn Dir | Case Dir Alts | CIdent Ident
data Dir     = Left | Right | Front
data Alt     = Alt Pat Cmds
data Pat     = Contents | Underscore

data Contents  = Empty | Lambda | Debris | Asteroid | Boundary

data Ident = Digit Int | Letter String | Plus Int Int | Minus Int Int
--type Ident = ()
