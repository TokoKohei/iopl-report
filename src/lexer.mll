{
let reservedWords = [
  (* Keywords *)
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
  ("in", Parser.IN);  (* 3.3.1 *)
  ("let", Parser.LET); (* 3.3.1 *)
  ("fun", Parser.FUN); (* 3.4.1 *)
  ("rec", Parser.REC); (* 3.5.1 *)
  ("print_string", Parser.PRINT_STRING);
  ("proj1", Parser.PROJ1); 
  ("proj2", Parser.PROJ2); 
]
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "*" { Parser.MULT }
| "<" { Parser.LT }
| "=" { Parser.EQ } (* 3.3.1 *)
| "&&"{ Parser.ANDAND } (* 3.2.3 *)
| "||"{ Parser.OROR } (* 3.2.3 *)
| "->" { Parser.RARROW } (* 3.4.1 *)
| "[" { Parser.LBRACKET } (* 3.6.2 *)
| "]" { Parser.RBRACKET } (* 3.6.2 *)
| ";" { Parser.SEMICOLON } (* 3.6.2 *)
| "::" { Parser.CONS } (* 3.6.2 *)
|"^" { Parser.CONCAT } (* s1^s2 *)
|".[" { Parser.DOT_LBRACKET } (* s[i] *)
|'"' [^'"']*'"' 
    { let s = Lexing.lexeme lexbuf in  
      let len = String.length s in 
      Parser.STRINGV (String.sub s 1 (len - 2)) }
| "," { Parser.COMMA }

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| eof { exit 0 }
