
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token IN
%token BOOL
%token NAT
%token STRING
%token UNIT
%token UNITV
%token LIST

%token LPAREN
%token RPAREN
%token LCURLY
%token RCURLY
%token COMMA
%token FSTPROJ
%token SNDPROJ
%token DOT
%token EQ
%token COLON
%token ARROW
%token EOF
%token CONCAT
%token RSQUARE
%token LSQUARE
%token NIL
%token CONS
%token ISNIL
%token HEAD
%token TAIL

%token <string> STRINGO
%token <int> INTV
%token <string> STRINGV

%start s
%type <Lambda.command> s

%%

s :
    STRINGV EQ term EOF
      { Bind ($1, $3) }
    | term EOF
      { Eval $1 }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA STRINGV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET STRINGV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC STRINGV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }

appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | appTerm atomicTerm
      { TmApp ($1, $2) }
  | LPAREN term COMMA term RPAREN
      { TmPair ($2, $4)}
  | appTerm FSTPROJ
      { TmPairFstProj $1 }
  | appTerm SNDPROJ
      { TmPairSndProj $1 }
  | atomicTerm CONCAT atomicTerm
      { TmConcat ($1, $3) }
  | NIL LSQUARE ty RSQUARE
      { TmNil $3 }
  | CONS LSQUARE ty RSQUARE atomicTerm atomicTerm
      { TmCons ($3, $5, $6) }
  | ISNIL LSQUARE ty RSQUARE atomicTerm
      { TmIsNil ($3, $5)}
  | HEAD LSQUARE ty RSQUARE atomicTerm
      { TmHead ($3, $5)}
  | TAIL LSQUARE ty RSQUARE atomicTerm
      { TmTail ($3, $5)}

    


atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | STRINGO
      { TmString $1 }
  | UNITV
      { TmUnit }
  | STRINGV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }
  | atomicTy LIST
      { TyList $1 }


atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyString }
  | UNIT
      { TyUnit }
