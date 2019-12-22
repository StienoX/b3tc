module Language where

import Prelude hiding (Left, Right)

type Program = [Rule]
data Rule    = Rule Ident Cmds deriving (Show)
type Cmds    = [Cmd]
data Cmd     = Go | Take | Mark | CNothing | Turn Dir | Case Dir Alts | CIdent Ident deriving (Show)
data Dir     = Left | Right | Front deriving (Show)
type Alts    = [Alt]
data Alt     = Alt Pat Cmds deriving (Show)
data Pat     = Empty | Lambda | Debris | Asteroid | Boundary | Underscore deriving (Eq, Ord, Show)

type Ident   = String

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

When using parser combinators, left-recursion results in an infinite loop when parsing.
Parser combinator vs Parser Generator (Happy).


Sources: 
[1] https://www.haskell.org/happy/doc/html/sec-sequences.html
[2] https://www.haskell.org/happy/doc/html/sec-glr-misc.html
-}

-- Exercise 5
type AlgProgram program rule cmd alt id = ( [rule] -> program        -- Program
                                          , id     -> [cmd] -> rule  -- Rule
                                          , (cmd, cmd, cmd, cmd, Dir -> cmd, Dir -> [alt] -> cmd, id -> cmd) -- Cmd
                                          , Dir                      -- Dir
                                          , Pat    -> [cmd] -> alt   -- Alt
                                          , Pat                      -- Pat
                                          , String -> id             -- Identifier
                                        )

{-
foldProgram :: AlgProgram program rule cmd alt id -> Program -> program
foldProgram (program', rule', cmd', turn', case', cident', alt', string') = foldProgram
  where
    foldProgram (x:xs)            = program' (foldRule x) (foldProgram xs) --Program
    foldRule    ident cmds        = rule' (foldIdent ident) (map foldCmd cmds) --Rule
    foldCmd     (Case dir alts)   = case' dir $ map foldAlt alts --Case
    foldCmd     (Turn dir)        = turn' dir
    foldCmd     (CIdent ident)    = cident' (foldIdent ident)
    foldCmd     _                 = cmd'
    foldAlt     pat cmds          = alt' pat $ map foldCmd cmds
    foldIdent   s                 = string' s
-}

foldProgram :: AlgProgram program rule cmd alt id -> Program -> program
foldProgram (program', rule', (go', take', mark', nothing', turn', case', cident'), dir', alt', pat', ident') = fold
  where
    fold        rules             = program' $ map foldRule rules
    foldRule    (Rule ident cmds) = rule' (foldIdent ident) $ map foldCmd cmds
    foldCmd     (Case dir alts)   = case' dir $ map foldAlt alts
    foldCmd     (Turn dir)        = turn' dir
    foldCmd     (CIdent ident)    = cident' (foldIdent ident)
    foldCmd     Go                = go'
    foldCmd     Take              = take'
    foldCmd     Mark              = mark'
    foldCmd     CNothing          = nothing'
    foldAlt     (Alt pat cmds)    = alt' pat $ map foldCmd cmds
    foldIdent   s                 = ident' s

