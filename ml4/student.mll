{
open Ml4common;;
}

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)

let numeric     = ['0' - '9']
let lowercase   = ['a' - 'z']
let letter      = ['a' - 'z' 'A' - 'Z' '_']
let hex = ['0' - '9' 'a' - 'f']
let bi = ['0' '1']

rule token = parse
    | [' ' '\t' '\n']   { token lexbuf }  (* skip over whitespace *)
    | eof               { EOF }

(* your rules go here *)
| "~"     { NEG }
  | "+"     { PLUS }
  | "-"     { MINUS }
  | "*"     { TIMES }
  | "/"     { DIV }
  | "+."    { DPLUS }
  | "-."    { DMINUS }
  | "*."    { DTIMES }
  | "/."    { DDIV }
  | "^"     { CARAT }
  | "<"     { LT }
  | ">"     { GT }
  | "<="    { LEQ }
  | ">="    { GEQ }
  | "="     { EQUALS }
  | "<>"    { NEQ }
  | "|"     { PIPE }
  | "->"    { ARROW }
  | ";"     { SEMI }
  | ";;"    { DSEMI }
  | "::"    { DCOLON }
  | "@"     { AT }
  | "[]"    { NIL }
  | "let"   { LET }
  | "rec"   { REC }
  | "and"   { AND }
  | "end"   { END }
  | "in"    { IN }
  | "if"    { IF }
  | "then"  { THEN }
  | "else"  { ELSE }
  | "fun"   { FUN }
  | "mod"   { MOD }
  | "raise" { RAISE }
  | "try"   { TRY }
  | "with"  { WITH }
  | "not"   { NOT }
  | "&&"    { LOGICALAND }
  | "||"    { LOGICALOR }
  | "["     { LBRAC }
  | "]"     { RBRAC }
  | "("     { LPAREN }
  | ")"     { RPAREN }
  | ","     { COMMA }
  | "_"     { UNDERSCORE }
  | "true"  { TRUE }
  | "false" { FALSE }
  | "()"    { UNIT }
  | numeric+ as s { INT (int_of_string s) }
  | "0b"bi+ as s { INT (int_of_string s) }
  | "0x"hex+ as s { INT (int_of_string s) }
  | numeric+"."numeric* as s { FLOAT (float_of_string s) }
  | numeric+"."numeric*"e"numeric+ as s { FLOAT (float_of_string s) }
  | lowercase+ as s { IDENT s }
  | lowercase+numeric+ as s { IDENT s }
  | lowercase+letter+ as s { IDENT s }
  | "'"letter*"'" as s { STRING s }


{
(* do not modify this function: *)
let lextest s = token (Lexing.from_string s)

let get_all_tokens s =
    let b = Lexing.from_string (s^"\n") in
    let rec g () =
        match token b with
            EOF -> []
            | t -> t :: g ()
        in
    g ()

let try_get_all_tokens s =
    try (Some (get_all_tokens s), true)
    with Failure "unmatched open comment" -> (None, true)
       | Failure "unmatched closed comment" -> (None, false)

let get_all_token_options s =
    let b = Lexing.from_string (s^"\n") in
    let rec g () =
        match (try Some (token b) with _ -> None) with
            Some EOF -> []
            | None -> [None]
            | t -> t :: g ()
        in
    g ()
}
