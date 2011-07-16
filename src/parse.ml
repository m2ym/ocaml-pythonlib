module Make (A : Ast.Annot) = struct
  module Parser = Parser.Make (A)

  let parse_from_lexbuf lexbuf =
    let state = Lexer_state.create () in
      Parser.file_input (Lexer.token state) lexbuf

  let parse_from_string s =
    parse_from_lexbuf (Lexing.from_string s)

  let parse_from_channel c =
    parse_from_lexbuf (Lexing.from_channel c)

  let parse_from_file file =
    let c = open_in file in
      try
        let ast = parse_from_channel c in
          close_in c;
          ast
      with e ->
        close_in c;
        raise e

  let parse_expr_from_lexbuf lexbuf =
    let state = Lexer_state.create () in
      Parser.expr (Lexer.token state) lexbuf

  let parse_expr_from_string s =
    parse_expr_from_lexbuf (Lexing.from_string s)

  let parse_expr_from_channel c =
    parse_expr_from_lexbuf (Lexing.from_channel c)
end

include Make (Ast.Pos)
