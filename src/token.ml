type token =
  | NAME of (string * Lexing.position)
  | INT of (int * Lexing.position)
  | LONGINT of (int * Lexing.position)
  | FLOAT of (float * Lexing.position)
  | IMAG of (string * Lexing.position)
  | STR of (string * Lexing.position)
  | INDENT
  | DEDENT
  | NEWLINE
  | AND of (Lexing.position)
  | AS of (Lexing.position)
  | ASSERT of (Lexing.position)
  | BREAK of (Lexing.position)
  | CLASS of (Lexing.position)
  | CONTINUE of (Lexing.position)
  | DEF of (Lexing.position)
  | DEL of (Lexing.position)
  | ELIF of (Lexing.position)
  | ELSE of (Lexing.position)
  | EXCEPT of (Lexing.position)
  | EXEC of (Lexing.position)
  | FINALLY of (Lexing.position)
  | FOR of (Lexing.position)
  | FROM of (Lexing.position)
  | GLOBAL of (Lexing.position)
  | IF of (Lexing.position)
  | IMPORT of (Lexing.position)
  | IN of (Lexing.position)
  | IS of (Lexing.position)
  | LAMBDA of (Lexing.position)
  | NOT of (Lexing.position)
  | OR of (Lexing.position)
  | PASS of (Lexing.position)
  | PRINT of (Lexing.position)
  | RAISE of (Lexing.position)
  | RETURN of (Lexing.position)
  | TRY of (Lexing.position)
  | WHILE of (Lexing.position)
  | WITH of (Lexing.position)
  | YIELD of (Lexing.position)
  | ADD of (Lexing.position)
  | SUB of (Lexing.position)
  | MULT of (Lexing.position)
  | DIV of (Lexing.position)
  | MOD of (Lexing.position)
  | POW of (Lexing.position)
  | FDIV of (Lexing.position)
  | BITOR of (Lexing.position)
  | BITAND of (Lexing.position)
  | BITXOR of (Lexing.position)
  | BITNOT of (Lexing.position)
  | LSHIFT of (Lexing.position)
  | RSHIFT of (Lexing.position)
  | EQ of (Lexing.position)
  | ADDEQ of (Lexing.position)
  | SUBEQ of (Lexing.position)
  | MULTEQ of (Lexing.position)
  | DIVEQ of (Lexing.position)
  | MODEQ of (Lexing.position)
  | POWEQ of (Lexing.position)
  | FDIVEQ of (Lexing.position)
  | ANDEQ of (Lexing.position)
  | OREQ of (Lexing.position)
  | XOREQ of (Lexing.position)
  | LSHEQ of (Lexing.position)
  | RSHEQ of (Lexing.position)
  | EQUAL of (Lexing.position)
  | NOTEQ of (Lexing.position)
  | LT of (Lexing.position)
  | GT of (Lexing.position)
  | LEQ of (Lexing.position)
  | GEQ of (Lexing.position)
  | LPAREN of (Lexing.position)
  | RPAREN of (Lexing.position)
  | LBRACK of (Lexing.position)
  | RBRACK of (Lexing.position)
  | LBRACE of (Lexing.position)
  | RBRACE of (Lexing.position)
  | COLON of (Lexing.position)
  | SEMICOL of (Lexing.position)
  | DOT of (Lexing.position)
  | COMMA of (Lexing.position)
  | BACKQUOTE of (Lexing.position)
  | AT of (Lexing.position)
  | ENDMARKER
