{
module Scanner where

import Prelude hiding (Left, Right)
}

%wrapper "basic"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters

tokens :-

    -- Ignore
    $white+                       ;
    "--".*                        ;

    -- Symbolic
    \;                            { \_ -> TokenSemicolon }
    "->"                          { \_ -> TokenRightArrow }
    \,                            { \_ -> TokenComma }
    \.                            { \_ -> TokenPeriod }

    -- Command Keywords
    go                            { \_ -> TokenGo }
    take                          { \_ -> TokenTake }
    mark                          { \_ -> TokenMark }
    nothing                       { \_ -> TokenNothing }
    turn                          { \_ -> TokenTurn }
    case                          { \_ -> TokenCase }
    of                            { \_ -> TokenOf }
    end                           { \_ -> TokenEnd }

    -- Pattern Keywords
    left                          { \_ -> TokenLeft }
    right                         { \_ -> TokenRight }
    front                         { \_ -> TokenFront }

    Empty                         { \_ -> TokenEmpty }
    Lambda                        { \_ -> TokenLambda }
    Debris                        { \_ -> TokenDebris }
    Asteroid                      { \_ -> TokenAsteroid }
    Boundary                      { \_ -> TokenBoundary }

    -- Identifier
    $alpha [$alpha $digit \+ \- \_ \’]* { \s -> TokenString s }

    -- Catch all
    \_                             { \_ -> TokenUnderscore }

{
-- Each action has type :: String -> Token

-- The token type:

data Token = TokenRightArrow
           | TokenPeriod
           | TokenComma
           | TokenGo
           | TokenTake
           | TokenMark
           | TokenNothing
           | TokenTurn
           | TokenCase
           | TokenOf
           | TokenEnd
           | TokenLeft
           | TokenRight
           | TokenFront
           | TokenSemicolon
           | TokenEmpty
           | TokenLambda
           | TokenDebris
           | TokenAsteroid
           | TokenBoundary
           | TokenUnderscore
           | TokenString String
           deriving (Eq,Show)

}
