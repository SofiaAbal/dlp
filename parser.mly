
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
    pathTerm
      { $1 }
  | SUCC pathTerm
      { TmSucc $2 }
  | PRED pathTerm
      { TmPred $2 }
  | ISZERO pathTerm
      { TmIsZero $2 }
  | appTerm pathTerm
      { TmApp ($1, $2) }
  | LCURLY term COMMA term RCURLY
      { TmPair ($2, $4)}
  | FSTPROJ appTerm
      { TmPairFstProj $2 }
  | SNDPROJ appTerm
      { TmPairSndProj $2 }
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
  | atomicTerm DOT STRINGV
      { TmProj ($1, $3) }

    
pathTerm :
  pathTerm DOT STRINGV
    { TmProj ($1, $3) }
  | pathTerm DOT INTV /* En las tuplas debería entrar por aquí, pero no lo pilla */
    { TmProj ($1, string_of_int $3) }
  | atomicTerm
    { $1 }

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
  | LCURLY tupleFields RCURLY
    { TmTuple $2}
  | LCURLY recordFields RCURLY
    { TmRecord $2}

tupleFields : 
  term 
    {[$1]}
  | term COMMA tupleFields
      {$1 :: $3}


recordFields :
    {[]}
  | STRINGV EQ term
    {[($1, $3)]}
  | STRINGV EQ term COMMA recordFields
    {($1, $3) :: $5}

/*
recordFields : 
   {[]}
  | notEmptyRecordFields
    {$1}

notEmptyRecordFields : 
  notEmptyRecordFieldType
    {[($1)]}
  | notEmptyRecordFieldType COMMA notEmptyRecordFieldType
    {$1 :: $3}

notEmptyRecordFieldType :
  | STRINGV EQ term
      { $3 }
*/

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
  | LCURLY tupleFieldsType RCURLY
      { TyTuple $2}
  | LCURLY recordFieldsType RCURLY
      { TyRecord $2}

tupleFieldsType : 
  ty 
    {[$1]}
  | ty COMMA tupleFieldsType
      {$1 :: $3}

recordFieldsType :
    {[]}
  | STRINGV EQ ty
    {[($1, $3)]}
  | STRINGV EQ ty COMMA recordFieldsType
    {($1, $3) :: $5}