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
    \;                            { \s -> TokenSemicolon }
    "->"                          { \s -> TokenRightArrow }
    \,                            { \s -> TokenComma }
    \.                            { \s -> TokenPeriod }

    -- Command Keywords
    go                            { \s -> TokenGo }
    take                          { \s -> TokenTake }
    mark                          { \s -> TokenMark }
    nothing                       { \s -> TokenNothing }
    turn                          { \s -> TokenTurn }
    case                          { \s -> TokenCase }
    of                            { \s -> TokenOf }
    end                           { \s -> TokenEnd }

    -- Pattern Keywords
    left                          { \s -> TokenLeft }
    right                         { \s -> TokenRight }
    front                         { \s -> TokenFront }

    Empty                         { \s -> TokenEmpty }
    Lambda                        { \s -> TokenLambda }
    Debris                        { \s -> TokenDebris }
    Asteroid                      { \s -> TokenAsteroid }
    Boundary                      { \s -> TokenBoundary }

    -- Identifier
    \+                            { \s -> TokenPlus }
    \-                            { \s -> TokenMinus }
    $digit+                       { \s -> TokenDigit (read s) }
    $alpha [$alpha $digit \_ \’]* { \s -> TokenLetter s }

    -- Catch all
    \_                             { \s -> TokenUnderscore }

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
           | TokenLetter String
           | TokenDigit Int
           | TokenPlus
           | TokenMinus
           deriving (Eq,Show)

main = do
    s <- getContents
    print (alexScanTokens s)
}
