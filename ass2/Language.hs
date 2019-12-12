module Language where

import Prelude hiding (Left, Right, Nothing)

data Program  = Program [Rule] deriving (Show)
type Cmds     = [Cmd]
type Alts     = [Alt]

data Rule     = Rule Ident Cmds deriving (Show)
data Cmd      = Go | Take | Mark | Nothing | Turn Dir | Case Dir Alts | CIdent Ident deriving (Show)
data Dir      = Left | Right | Front deriving (Show)
data Alt      = Alt Pat Cmds deriving (Show)
data Pat      = Contents | Underscore deriving (Show)

data Contents = Empty | Lambda | Debris | Asteroid | Boundary deriving (Show)

data Ident = Digit Int | Letter String | Plus Int Int | Minus Int Int deriving (Show)
--type Ident = ()

-- Exercise 5
type AlgProgram rules rule id cmd alt = ( rules -> rule            -- Program
                                        , rule -> id -> [cmd]
                                        , cmd
                                        , cmd -> Dir
                                        , cmd -> Dir
)

--foldProgram :: AlgProgram a -> Program -> a
--foldProgram = undefined
{-

Exercise 4
----------
What can you find out from the Happy documentation over Happy’s handling of left-recursive and right-recursive grammars.

Happy is more efficient when handling left-recursive rules because they result in a constant stack-space parser.
The right-recursive rules require stack space proportional to the length of the list being parsed,
this is especially problematic with long sequences, as these can fail due to a lack of stack space [1].

The current Happy code implements the standard GLR algorithm extended to handle hidden left recursion.
Cyclic grammers, however, are not supported [2].

How does this compare to the situation when using parser combinators?

When using parser combinators, left-recursion can result in an infinite loop when parsing.
Parser combinator vs Parser Generator (Happy).


Sources: 
[1] https://www.haskell.org/happy/doc/html/sec-sequences.html
[2] https://www.haskell.org/happy/doc/html/sec-glr-misc.html

-}