{
  open Lexer_state
  open Lexing
  open Token

  let curr_pos lexbuf = lexbuf.lex_start_p

  let unescaped s =
    let buf = Buffer.create (String.length s) in
    let escape = ref false in
    let unescapechar c =
      if !escape then begin
        match c with
        | '\r' -> ()
        | '\n' -> escape := false
        | _ -> begin
            escape := false;
            (* TODO http://docs.python.org/reference/lexical_analysis.html#string-literals *)
            Buffer.add_char
              buf
              (match c with
               | '\\' -> '\\'
               | '\'' -> '\''
               | '"' -> '"'
               | 'a' -> Char.chr 7
               | 'b' -> '\b'
               | 'f' -> Char.chr 12
               | 'n' -> '\n'
               | 'r' -> '\r'
               | 't' -> '\t'
               | 'v' -> Char.chr 11
               | _ -> (Buffer.add_char buf '\\'; c))
          end
      end else if c = '\\' then
        escape := true
      else
        Buffer.add_char buf c
    in
      String.iter unescapechar s;
      Buffer.contents buf

  let count_lines s =
    let n = ref 0 in
      String.iter
        (fun c -> if c = '\n' then incr n)
        s;
      !n
}

(* epsilon *)
let e = ""

let newline = ('\n' | "\r\n")
let whitespace = [' ' '\t']
let comment = '#' [^ '\n' '\r']*

let digit = ['0'-'9']
let octdigit = ['0'-'7']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let nonzerodigit = ['1'-'9']
let longintpostfix = ['l' 'L']
let decimalinteger = nonzerodigit digit*
let octinteger = '0' octdigit+
let hexinteger = '0' ['x' 'X'] hexdigit+
let intpart = digit+
let fraction = '.' digit+
let pointfloat = intpart? fraction | intpart '.'
let exponent = ['e' 'E'] ['+' '-']? digit+
let exponentfloat = (intpart | pointfloat) exponent
let floatnumber = pointfloat | exponentfloat
let imagnumber = (floatnumber | intpart) ['j' 'J']

let stringprefix = ('u' | 'U')? ('r' | 'R')?
let escapeseq = '\\' _

let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let nonidchar = [^ 'a'-'z' 'A'-'Z' '0'-'9' '_']

rule token state = parse
  | e { let curr_offset = state.curr_offset in
        let last_offset = Stack.top state.offset_stack in
          if curr_offset < last_offset
          then (ignore (Stack.pop state.offset_stack); DEDENT)
          else if curr_offset > last_offset
          then (Stack.push curr_offset state.offset_stack; INDENT)
          else _token state lexbuf }

and _token state = parse
  | ((whitespace* comment? newline)* whitespace* comment?) newline
      { let lines = count_lines (lexeme lexbuf) in
        let pos = lexbuf.lex_curr_p in
          lexbuf.lex_curr_p <-
            { pos with
                pos_bol = pos.pos_cnum;
                pos_lnum = pos.pos_lnum + lines };
        if state.nl_ignore <= 0 then begin
          state.curr_offset <- 0;
          offset state lexbuf;
          NEWLINE
        end else
          _token state lexbuf }
  | '\\' newline whitespace*
      { let pos = lexbuf.lex_curr_p in
          lexbuf.lex_curr_p <-
            { pos with
                pos_bol = pos.pos_cnum;
                pos_lnum = pos.pos_lnum + 1 };
          _token state lexbuf }

  | whitespace+
      { _token state lexbuf }

  (* keywords *)
  | identifier as id
      { match id with
        | "and"      -> AND (curr_pos lexbuf)
        | "as"       -> AS (curr_pos lexbuf)
        | "assert"   -> ASSERT (curr_pos lexbuf)
        | "break"    -> BREAK (curr_pos lexbuf)
        | "class"    -> CLASS (curr_pos lexbuf)
        | "continue" -> CONTINUE (curr_pos lexbuf)
        | "def"      -> DEF (curr_pos lexbuf)
        | "del"      -> DEL (curr_pos lexbuf)
        | "elif"     -> ELIF (curr_pos lexbuf)
        | "else"     -> ELSE (curr_pos lexbuf)
        | "except"   -> EXCEPT (curr_pos lexbuf)
        | "exec"     -> EXEC (curr_pos lexbuf)
        | "finally"  -> FINALLY (curr_pos lexbuf)
        | "for"      -> FOR (curr_pos lexbuf)
        | "from"     -> FROM (curr_pos lexbuf)
        | "global"   -> GLOBAL (curr_pos lexbuf)
        | "if"       -> IF (curr_pos lexbuf)
        | "import"   -> IMPORT (curr_pos lexbuf)
        | "in"       -> IN (curr_pos lexbuf)
        | "is"       -> IS (curr_pos lexbuf)
        | "lambda"   -> LAMBDA (curr_pos lexbuf)
        | "not"      -> NOT (curr_pos lexbuf)
        | "or"       -> OR (curr_pos lexbuf)
        | "pass"     -> PASS (curr_pos lexbuf)
        | "print"    -> PRINT (curr_pos lexbuf)
        | "raise"    -> RAISE (curr_pos lexbuf)
        | "return"   -> RETURN (curr_pos lexbuf)
        | "try"      -> TRY (curr_pos lexbuf)
        | "while"    -> WHILE (curr_pos lexbuf)
        | "with"     -> WITH (curr_pos lexbuf)
        | "yield"    -> YIELD (curr_pos lexbuf)
        | _          -> NAME (id, (curr_pos lexbuf)) }

  (* symbols *)
  | "+="    { ADDEQ (curr_pos lexbuf) }
  | "-="    { SUBEQ (curr_pos lexbuf) }
  | "*="    { MULTEQ (curr_pos lexbuf) }
  | "/="    { DIVEQ (curr_pos lexbuf) }
  | "%="    { MODEQ (curr_pos lexbuf) }
  | "**="   { POWEQ (curr_pos lexbuf) }
  | "//="   { FDIVEQ (curr_pos lexbuf) }
  | "&="    { ANDEQ (curr_pos lexbuf) }
  | "|="    { OREQ (curr_pos lexbuf) }
  | "^="    { XOREQ (curr_pos lexbuf) }
  | "<<="   { LSHEQ (curr_pos lexbuf) }
  | ">>="   { RSHEQ (curr_pos lexbuf) }

  | "=="    { EQUAL (curr_pos lexbuf) }
  | "!="    { NOTEQ (curr_pos lexbuf) }
  | "<>"    { NOTEQ (curr_pos lexbuf) }
  | "<="    { LEQ (curr_pos lexbuf) }
  | ">="    { GEQ (curr_pos lexbuf) }
  | '<'     { LT (curr_pos lexbuf) }
  | '>'     { GT (curr_pos lexbuf) }

  | '='     { EQ (curr_pos lexbuf) }

  | "**"    { POW (curr_pos lexbuf) }
  | "//"    { FDIV (curr_pos lexbuf) }
  | '+'     { ADD (curr_pos lexbuf) }
  | '-'     { SUB (curr_pos lexbuf) }
  | '*'     { MULT (curr_pos lexbuf) }
  | '/'     { DIV (curr_pos lexbuf) }
  | '%'     { MOD (curr_pos lexbuf) }
  | '|'     { BITOR (curr_pos lexbuf) }
  | '&'     { BITAND (curr_pos lexbuf) }
  | '^'     { BITXOR (curr_pos lexbuf) }
  | '~'     { BITNOT (curr_pos lexbuf) }
  | "<<"    { LSHIFT (curr_pos lexbuf) }
  | ">>"    { RSHIFT (curr_pos lexbuf) }

  | '('     { ignore_nl state; LPAREN (curr_pos lexbuf) }
  | ')'     { aware_nl state; RPAREN (curr_pos lexbuf) }
  | '['     { ignore_nl state; LBRACK (curr_pos lexbuf) }
  | ']'     { aware_nl state; RBRACK (curr_pos lexbuf) }
  | '{'     { ignore_nl state; LBRACE (curr_pos lexbuf) }
  | '}'     { aware_nl state; RBRACE (curr_pos lexbuf) }
  | ':'     { COLON (curr_pos lexbuf) }
  | ';'     { SEMICOL (curr_pos lexbuf) }
  | '.'     { DOT (curr_pos lexbuf) }
  | ','     { COMMA (curr_pos lexbuf) }
  | '`'     { BACKQUOTE (curr_pos lexbuf) }
  | '@'     { AT (curr_pos lexbuf) }

  (* literals *)
  | decimalinteger as n longintpostfix
      { LONGINT (int_of_string n, curr_pos lexbuf) }
  | decimalinteger as n
      { INT (int_of_string n, curr_pos lexbuf) }
  | octinteger as n longintpostfix
      { LONGINT (int_of_string ("0o" ^ n), curr_pos lexbuf) }
  | octinteger as n
      { INT (int_of_string ("0o" ^ n), curr_pos lexbuf) }
  | hexinteger as n longintpostfix
      { LONGINT (int_of_string n, curr_pos lexbuf) }
  | hexinteger as n
      { INT (int_of_string n, curr_pos lexbuf) }
  | floatnumber as n
      { FLOAT (float_of_string n, curr_pos lexbuf) }
  | imagnumber as n
      { IMAG (n, curr_pos lexbuf) }
  | '0' longintpostfix
      { LONGINT (0, curr_pos lexbuf) }
  | '0'
      { INT (0, curr_pos lexbuf) }

  | stringprefix '\''
      { sq_shortstrlit state (curr_pos lexbuf) lexbuf }
  | stringprefix '"'
      { dq_shortstrlit state (curr_pos lexbuf) lexbuf }
  | stringprefix "'''"
      { sq_longstrlit state (curr_pos lexbuf) lexbuf }
  | stringprefix "\"\"\""
      { dq_longstrlit state (curr_pos lexbuf) lexbuf }

  (* eof *)
  | eof { ENDMARKER }

and offset state = parse
  | e { }
  | ' '  { state.curr_offset <- state.curr_offset + 1; offset state lexbuf }
  | '\t' { state.curr_offset <- state.curr_offset + 8; offset state lexbuf }

and sq_shortstrlit state pos = parse
  | (([^ '\\' '\r' '\n' '\''] | escapeseq)* as s) '\'' { STR (unescaped s, pos) }

and sq_longstrlit state pos = shortest
| (([^ '\\'] | escapeseq)* as s) "'''"
    { let lines = count_lines s in
      let curpos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { curpos with pos_lnum = curpos.pos_lnum + lines };
        STR (unescaped s, pos) }

and dq_shortstrlit state pos = parse
  | (([^ '\\' '\r' '\n' '\"'] | escapeseq)* as s) '"' { STR (unescaped s, pos) }

and dq_longstrlit state pos = shortest
  | (([^ '\\'] | escapeseq)* as s) "\"\"\""
      { let lines = count_lines s in
        let curpos = lexbuf.lex_curr_p in
          lexbuf.lex_curr_p <- { curpos with pos_lnum = curpos.pos_lnum + lines };
          STR (unescaped s, pos) }
