{
module Parser where

import Prelude hiding (Left, Right, Nothing)

import Language
import Scanner

}

%name parseProgram
%tokentype { Token }
%error { parseError }

%token
"->"            { TokenRightArrow }
'.'             { TokenPeriod     }
','             { TokenComma      }
go              { TokenGo         }
take            { TokenTake       }
mark            { TokenMark       }
nothing         { TokenNothing    }
turn            { TokenTurn       }
case            { TokenCase       }
of              { TokenOf         }
end             { TokenEnd        }
left            { TokenLeft       }
right           { TokenRight      }
front           { TokenFront      }
';'             { TokenSemicolon  }
empty           { TokenEmpty      }
lambda          { TokenLambda     }
debris          { TokenDebris     }
asteroid        { TokenAsteroid   }
boundary        { TokenBoundary   }
'_'             { TokenUnderscore }
letter          { TokenLetter $$  }
digit           { TokenDigit $$   }
'+'             { TokenPlus       }
'-'             { TokenMinus      }

%%

Program :: { Program }
Program :  Rules             { Program $1 }

Rules :: { [Rule] }
Rules : {- empty -}          { []      }
      | Rules Rule           { $2 : $1 }

Rule  :: { Rule }
Rule  : Ident "->" Cmds '.'  { Rule $1 $3 }

Cmds  :: { [Cmd] } 
Cmds  : {- empty -}          { []      } 
      | Cmd ',' Cmds         { $1 : $3 }
      | Cmd                  { [$1]    }

Cmd   :: { Cmd } 
Cmd   : go                   { Go         }
      | take                 { Take       }
      | mark                 { Mark       }
      | nothing              { Nothing    }
      | turn Dir             { Turn $2    }
      | case Dir of Alts end { Case $2 $4 }
      | Ident                { CIdent $1  }

Dir   :: { Dir }
Dir   : left                 { Left  }
      | right                { Right }
      | front                { Front }

Alts  :: { [Alt] }
Alts  : {- empty -}          { []      } 
      | Alt ';' Alts         { $1 : $3 }
      | Alt                  { [$1]    }

Alt   :: { Alt }
Alt   : Pat "->" Cmds        { Alt $1 $3 }

Pat   :: { Pat }
Pat   : Contents             { Contents }
      | '_'                  { Underscore }

Contents :: { Contents }
Contents  : empty            { Empty    }
          | lambda           { Lambda   }
          | debris           { Debris   }
          | asteroid         { Asteroid }
          | boundary         { Boundary }

Ident :: { Ident }
Ident : letter               { Letter $1    }
      | digit                { Digit  $1    }
      | digit '+' digit      { Plus   $1 $3 }
      | digit '-' digit      { Minus  $1 $3 }

{
    
-- Happy parse error.
parseError :: [Token] -> a
parseError = error "Parse error, happy niet blij."

main = getContents >>= \_ -> return $ parseProgram . alexScanTokens

}