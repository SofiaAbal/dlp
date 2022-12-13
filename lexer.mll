
{
  open Parser;;
  exception Lexical_error;; 
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"    { LETREC}
  | "in"        { IN }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "String"    { STRING }
  | "Unit"      { UNIT }
  | "()"        { UNITV }
  | "unit"      { UNITV }
  | "list"      { LIST }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         { LCURLY }
  | '}'         { RCURLY }
  | ','         { COMMA }
  | "first"     { FSTPROJ }
  | "second"    { SNDPROJ }
  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | ';'         { SEMICOLON }
  | "->"        { ARROW }
  | '^'         { CONCAT }
  | "nil"       { NIL }
  | "cons"      { CONS }
  | '['         { LSQUARE }
  | ']'         { RSQUARE }
  | "isnil"     { ISNIL }
  | "head"      { HEAD }
  | "tail"      { TAIL }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { STRINGV (Lexing.lexeme lexbuf) }
  | '"'[^ '"' ';' '\n']*'"' 
                { let s = Lexing.lexeme lexbuf in STRINGO (String.sub s 1 (String.length s - 2)) }
  | eof         { EOF }
  | _           { raise Lexical_error } 

