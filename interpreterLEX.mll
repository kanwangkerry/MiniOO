(* File interpreterLEX.mll *)
{
open InterpreterYACC;; (* Type token defined in interpreterYACC.mli *)
exception Eof;;
}
rule token = parse
[' ' '\t' '\n'] { if lexbuf.lex_buffer.[lexbuf.lex_curr_pos-2] = ';' then begin print_int lexbuf.lex_curr_pos; print_char ' '; print_char
lexbuf.lex_buffer.[lexbuf.lex_curr_pos-2]; end; token lexbuf } (*skip blanks,
tabs*)
  | ['\n' ]    { EOL }
  | 't' 'r' 'u' 'e'         {BTRUE} 
  | 'f' 'a' 'l' 's' 'e'     {BFALSE}
  | 'v' 'a' 'r'             {print_string "lex_var" ; print_newline(); flush
  stdout; CVAR}
  | 'p' 'r' 'o' 'c'         {PROC}
  | 'e' 'n' 'd' 'p' 'r' 'o' 'c' {ENDPROC}
  | 'm' 'a' 'l' 'l' 'o' 'c' {CMALLOC}
  | 's' 'k' 'i' 'p'         {CSKIP}
  | 'w' 'h' 'i' 'l' 'e'     {CWHILE}
  | 'e' 'n' 'd'             {CEND}
  | 'i' 'f'                 {CIF}
  | 't' 'h' 'e' 'n'         {CTHEN}
  | 'e' 'l' 's' 'e'         {CELSE}
  | 'e' 'n' 'd' 'i' 'f'     {CENDIF}
  | '|' '|' '|'             {CPARALLEL}
  | 'a' 't' 'o' 'm'         {CATOM}
  | 'n' 'u' 'l' 'l'         {ENULL}
  (*These lex rule should be before the var definition because they might be
   * hidden behind the idt (var)*)
  | (['a'-'z'] | ['A'-'Z'])(['a'-'z'] | ['A'-'Z'] | ['0'-'9'])* as idt
  { if lexbuf.lex_buffer.[lexbuf.lex_curr_pos-2] = '.' then begin print_string
  "field"; print_newline; flush stdout; FIELD idt end else VAR idt }
  | ['0'-'9']+ as num
  { NUM (int_of_string num) }
  | ';'         { SEMICOLON }
  | ':'         { COLON }
  | '='         { ASSIGN }
  | '+'         { PLUS }
  | '-'         { MINUS }
  | '*'         { TIMES }
  | '/'         { DIV }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         { LBRACKET}
  | '}'         { RBRACKET}
  | '=' '='     { EQUAL }
  | '<'         { LESS }
  | '>'         { GREATER }
  | '.'         { POINT }
  | eof         { raise Eof }
