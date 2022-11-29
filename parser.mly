
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

%token LPAREN
%token RPAREN
%token DOT
%token EQ
%token COLON
%token ARROW
%token EOF

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

pathTerm :
  pathTerm DOT STRINGV
    { TmProj ($1, $3) }
  | pathTerm DOT INTV 
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
    { TmTuple $2}

tupleFields : 
  term 
    {[$1]}
    | term COMMA tupleFields
      {$1 :: $3}

recordFields : 
  /* Empty*/
   {[]}
  | notEmptyRecordFields
    {$1}

notEmptyRecordFields : 
  notEmptyRecordFieldType
    {[$1]}
  | notEmptyRecordFieldType COMMA notEmptyRecordFieldType
    {$1 :: $3}



ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | LCURLY tupleFieldTypes RCURLY
    { TyTuple $2}
  | LCURLY recordFieldTypes RCURLY
    { TyRecord $2}

