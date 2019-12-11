{
module Parser ( parseArrow ) where

import Prelude hiding (Left, Right, Nothing)

import Language
import Scanner

}

%name parseProgram
%tokentype { Token }

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

Program :: { [Rule] }
Program : {- empty -}        { []      }
        | Rule Program       { $1 : $2 }

Rule  :: { Rule }
Rule  : Ident "->" Cmds '.'  { $1 $3   }

Cmds  :: { [Cmd] } 
Cmds  : {- empty -}          { []      } 
      | Cmd ',' Cmds         { $1 : $3 }
      | Cmd                  { $1      }

Cmd   :: { Cmd } 
Cmd   : go                   { Go      }
      | take                 { Take    }
      | mark                 { Mark    }
      | nothing              { Nothing }
      | turn Dir             { $2      }
      | case Dir of Alts end { $2 $4   }
      | Ident                { $1      }

Dir   :: { Dir }
Dir   : left                 { Left  }
      | right                { Right }
      | front                { Front }

Alts  :: { [Alt] }
Alts  : {- empty -}          { []      } 
      | Alt ';' Alts         { $1 : $3 }
      | Alt                  { $1      }

Alt   :: { Alt }
Alt   : Pat                  { $1  }
      | Cmds                 { $1 }

Pat   :: { Pat }
Pat   : Contents             { $1 }
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