{
module Parser where

import Prelude hiding (Left, Right)

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
string          { TokenString $$  }

%%

Program :: { [Rule] }
Program : {- empty -}        { []      }
      | Program Rule         { $2 : $1 }

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
      | nothing              { CNothing   }
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
Pat   : empty                { PEmpty    }
      | lambda               { PLambda   }
      | debris               { PDebris   }
      | asteroid             { PAsteroid }
      | boundary             { PBoundary }
      | '_'                  { PUnderscore }

Ident :: { String }
Ident : string               { $1 }

{

-- Happy parse error.
parseError :: [Token] -> a
parseError = error "Parse error, happy niet blij."

--main = getContents >>= \_ -> return $ parseProgram . alexScanTokens

}