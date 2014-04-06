/* File interpreterYACC.mly */

%{ (* header *)
(* *)
exception ParseError ;;

(*Definition of Tree data structure*)
type label = string ;;
type content = CString of string | CInt of int ;;
type nodeVal = label * content
type node = Node of nodeVal * node list ref;;

let construct l c = 
    Node((l, c), ref [])

let add_node root child =
    match root with
        Node(v, l) -> l := !l@[child] 
    |   _ -> raise ParseError ;;

%} /* declarations */


%token EOL SEMICOLON COLON ASSIGN PLUS /* lexer tokens */
%token MINUS TIMES DIV LPAREN RPAREN
%token LBRACKET RBRACKET POINT
%token EQUAL LESS GREATER
%token BTRUE BFALSE PROC ENDPROC
%token CVAR CMALLOC CSKIP
%token CWHILE CEND
%token CIF CTHEN CELSE CENDIF
%token CPARALLEL CATOM ENULL
%token < string > VAR FIELD
%token < int > NUM
%start prog                   /* the entry point */
%type <unit> prog  
%type <node> cmd
%type <node> expr
%type <node> bool_expr
%left SEMICOLON
%left LESS GREATER EQUAL
%left ASSIGN
%left PLUS MINUS            /* lowest precedence */
%left TIMES DIV             /* medium precedence */
%left POINT
%nonassoc UMINUS           /* highest precedence */

%% /* rules */

prog :
    cmd SEMICOLON { let root = construct "prog" (CString("")); let c1 = $1; 
    let c2 = construct "SEMICOLON" (CString("")); add_node root c1;
    add_node root c2;
    print "tree builded";
    print_newline(); flush stdout; () }
	
  
cmd :
    CVAR VAR                    { let root = construct "cmd" (CString("")); 
    let c1 = construct "CVAR" (CString("")) ; let c2 = construct "VAR"
    (CString("$2")); 
    add_node root c1; add_node root c2; root}

|   expr LPAREN expr RPAREN     { 
    let root = construct "cmd" (CString(""));
    let c1 = $1 ; let c2 = construct "LPAREN" (CString("")); 
    let c3 = $3 ; let c4 = construct "RLAREN" (CString(""));
    add_node root c1; add_node root c2; add_node root c3; add_node root c4;
    root}

|   CMALLOC LPAREN expr RPAREN  { 
    let root = construct "cmd" (CString(""));
    let c1 = construct "CMALLOC" (CString(""));
    let c2 = construct "LPAREN" (CString(""));
    let c3 = $3;
    let c4 = construct "RPAREN" (CString(""));
    add_node root c1; add_node root c2; add_node root c3; add_node root c4;
    root
    }

|   VAR ASSIGN expr             {
    let root = construct "cmd" (CString(""));
    let c1 = construct "VAR" (CString($1));
    let c2 = construct "ASSIGN" (CString(""));
    let c3 = $3;
    add_node root c1; add_node root c2; add_node root c3; 
    root
    }

|   expr POINT expr ASSIGN expr { 
    let root = construct "cmd" (CString(""));
    let c1 = $1
    let c2 = construct "POINT" (CString(""));
    let c3 = $3;
    let c4 = construct "ASSIGN" (CString(""));
    let c5 = $5;
    add_node root c1; add_node root c2; add_node root c3; add_node root c4;
    add_node root c5;
    root
    }

|   CSKIP                       {
    let root = construct "cmd" (CString(""));
    let c1 = construct "CSKIP" (CString(""));
    add_node root c1; 
    root
    }

|   cmd SEMICOLON cmd           { 
    let root = construct "cmd" (CString(""));
    let c1 = $1
    let c2 = construct "SEMICOLON" (CString(""));
    let c3 = $3;
    add_node root c1; add_node root c2; add_node root c3; 
    root
    }

|   CWHILE bool_expr cmd SEMICOLON CEND   { 
    let root = construct "cmd" (CString(""));
    let c1 = construct "CWHILE" (CString(""));
    let c2 = $2;
    let c3 = $3;
    let c4 = construct "SEMICOLON" (CString(""));
    let c5 = construct "CEND" (CString(""));
    add_node root c1; add_node root c2; add_node root c3; add_node root c4;
    add_node root c5;
    root
    }
|   CIF bool_expr CTHEN cmd SEMICOLON CELSE cmd SEMICOLON CENDIF  {
    let root = construct "cmd" (CString(""));
    let c1 = construct "CIF" (CString(""));
    let c2 = $2;
    let c3 = construct "CTHEN" (CString(""));
    let c4 = $4;
    let c5 = construct "SEMICOLON" (CString(""));
    let c6 = construct "CELSE" (CString(""));
    let c7 = $7;
    let c8 = construct "SEMICOLON" (CString(""));
    let c9 = construct "CENDIF" (CString(""));
    add_node root c1; add_node root c2; add_node root c3; add_node root c4;
    add_node root c5; add_node root c6; add_node root c7; add_node root c8;
    add_node root c9;
    root
    }

|   LBRACKET cmd CPARALLEL cmd RBRACKET { 
    let root = construct "cmd" (CString(""));
    let c1 = construct "LBRACKET" (CString(""));
    let c2 = $2;
    let c3 = construct "CPARALLEL" (CString(""));
    let c4 = $4;
    let c5 = construct "RBRACKET" (CString(""));
    add_node root c1; add_node root c2; add_node root c3; add_node root c4;
    add_node root c5; 
    root

}

|   CATOM LPAREN cmd RPAREN     {
    let root = construct "cmd" (CString(""));
    let c1 = construct "CATOM" (CString(""));
    let c2 = construct "LPAREN" (CString(""));
    let c3 = $3;
    let c4 = construct "RPAREN" (CString(""));
    add_node root c1; add_node root c2; add_node root c3; add_node root c4;
    root
}
  
	
expr :
    FIELD           { () }
|   NUM             { print_string "NUM" }
|   expr PLUS expr           { () }
|   expr MINUS expr          { () }
|   expr TIMES expr          { () }
|   expr DIV expr            { () }
|   MINUS expr %prec UMINUS  { () }
|   ENULL           { () }
|   VAR             { print_string "e_var" ; print_newline(); flush stdout; () }
|   expr POINT expr { () }
|   PROC VAR COLON cmd SEMICOLON ENDPROC    { () }

bool_expr :
    BTRUE           { () }
|   BFALSE          { () }
|   expr EQUAL expr { () }
|   expr LESS expr  { print_string "LESS" }
|   expr GREATER expr   {()}
  
%% (* trailer *)
