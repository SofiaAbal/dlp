open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;
open String;;


let rec read_multi_lines () =
  let line = trim(read_line()) in
    if (contains line ';' && length line > 0) then sub line 0 (index line ';') (* we ignore everything from ';' *)
    else line ^ " " ^ (read_multi_lines ())
;;


let top_level_loop () =
  print_endline "   Lambda calculus Interpreter version 1.0\n";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      let tm = s token (from_string (read_multi_lines ())) in
      let tyTm = typeof ctx tm in
      print_endline (string_of_term (eval tm) ^ " : " ^ string_of_ty tyTm);
      loop ctx
    with
       Lexical_error ->
         print_endline "lexical error";
         loop ctx
     | Parse_error ->
         print_endline "syntax error";
         loop ctx
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop ctx
     | End_of_file ->
         print_endline "\n...closing interpreter\nBye!"
  in
    loop emptyctx
  ;;

top_level_loop ()
;;
