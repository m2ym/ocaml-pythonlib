open Format
open Ast
open Utils

let rec pp_char fmt c =
  pp_string fmt (String.make 1 c)

and pp_string fmt s =
  pp_print_as fmt 0 s

and pp_int fmt int =
  pp_string fmt (string_of_int int)

and pp_float fmt float =
  pp_string fmt (string_of_float float)

and pp_symbol fmt sym =
  pp_string fmt sym;
  pp_char fmt ' '

and pp_separated ?(delim=", ") fmt f = function
  | [] -> ()
  | [x] -> f x
  | x :: xs ->
      f x;
      pp_string fmt delim;
      pp_separated ~delim fmt f xs

and pp_cut fmt =
  pp_print_cut fmt ()

and pp_newline fmt =
  pp_force_newline fmt ()

and pp_colon fmt =
  pp_char fmt ':'

and pp_open fmt =
  pp_open_box fmt 4

and pp_close fmt =
  pp_close_box fmt ()

let rec pp_print_mod fmt = function
  | Module body
  | Interactive body
  | Suite body ->
      pp_print_body fmt body
  | Expression expr ->
      pp_print_expr fmt expr

and pp_print_stmt fmt = function
  | FunctionDef (name, args, body, decorator_list, _) ->
      pp_print_decorator_list fmt decorator_list;
      pp_symbol fmt "def";
      pp_string fmt name;
      pp_print_arguments fmt args;
      pp_colon fmt;
      pp_print_suite fmt body

  | ClassDef (name, bases, body, decorator_list, _) ->
      pp_print_decorator_list fmt decorator_list;
      pp_symbol fmt "class";
      pp_string fmt name;
      pp_print_expr_list fmt bases;
      pp_colon fmt;
      pp_print_suite fmt body

  | Return (None, _) ->
      pp_string fmt "return"
  | Return (Some value, _) ->
      pp_symbol fmt "return";
      pp_print_expr fmt value

  | Delete (targets, _) ->
      pp_symbol fmt "del";
      pp_print_expr_list ~paren:false fmt targets

  | Assign (targets, value, _) ->
      pp_print_expr_list ~paren:false fmt targets;
      pp_string fmt " = ";
      pp_print_expr fmt value

  | AugAssign (target, op, value, _) ->
      pp_print_expr fmt target;
      pp_char fmt ' ';
      pp_string fmt (string_of_operator op);
      pp_string fmt "= ";
      pp_print_expr fmt value

  | Print (dest, values, nl, _) ->
      pp_symbol fmt "print";
      (match dest with
       | None -> ()
       | Some d ->
           pp_symbol fmt ">>";
           pp_print_expr fmt d;
           if values <> [] then
             pp_symbol fmt ",");
      if values <> [] then
        pp_print_expr_list ~paren:false fmt values;
      if not nl then
        pp_char fmt ','

  | For (target, iter, body, orelse, _) ->
      pp_symbol fmt "for";
      pp_print_expr fmt target;
      pp_char fmt ' ';
      pp_symbol fmt "in";
      pp_print_expr fmt iter;
      pp_colon fmt;
      pp_print_suite fmt body;
      pp_print_orelse fmt orelse

  | While (test, body, orelse, _) ->
      pp_symbol fmt "while";
      pp_print_expr fmt test;
      pp_colon fmt;
      pp_print_suite fmt body;
      pp_print_orelse fmt orelse

  | If (test, body, orelse, _) ->
      pp_symbol fmt "if";
      pp_print_expr fmt test;
      pp_colon fmt;
      pp_print_suite fmt body;
      pp_print_if_orelse fmt orelse

  | With (context_expr, optional_vars, body, _) ->
      pp_symbol fmt "with";
      pp_print_expr fmt context_expr;
      (match optional_vars with
       | None -> ()
       | Some var ->
           pp_string fmt " as ";
           pp_print_expr fmt var);
      pp_colon fmt;
      pp_print_suite fmt body

  | Raise (typ, inst, tback, _) ->
      pp_string fmt "raise";
      (match typ with
       | None -> ()
       | Some typ ->
           pp_char fmt ' ';
           pp_print_expr fmt typ;
           match inst with
           | None -> ()
           | Some inst ->
               pp_symbol fmt ",";
               pp_print_expr fmt inst;
               match tback with
               | None -> ()
               | Some tback ->
                   pp_symbol fmt ",";
                   pp_print_expr fmt tback)

  | TryExcept (body, handlers, orelse, _) ->
      pp_string fmt "try";
      pp_colon fmt;
      pp_print_suite fmt body;
      pp_print_excepthandler_list fmt handlers;
      pp_print_orelse fmt orelse

  | TryFinally ([TryExcept _ as try_except], finalbody, _) ->
      pp_print_stmt fmt try_except;
      pp_newline fmt;
      pp_string fmt "finally";
      pp_colon fmt;
      pp_print_suite fmt finalbody
  | TryFinally (body, finalbody, _) ->
      pp_string fmt "try";
      pp_colon fmt;
      pp_print_suite fmt body;
      pp_newline fmt;
      pp_string fmt "finally";
      pp_colon fmt;
      pp_print_suite fmt finalbody

  | Assert (test, msg, _) ->
      pp_symbol fmt "assert";
      pp_print_expr fmt test;
      (match msg with
       | None -> ()
       | Some msg ->
           pp_symbol fmt ",";
           pp_print_expr fmt msg)

  | Import (names, _) ->
      pp_symbol fmt "import";
      pp_print_alias_list fmt names;

  | ImportFrom (modl, names, level, _) ->
      pp_symbol fmt "from";
      (match level with
       | None -> ()
       | Some level ->
           pp_string fmt (String.make level '.'));
      pp_symbol fmt modl;
      pp_symbol fmt "import";
      pp_print_alias_list fmt names

  | Exec (body, globals, locals, _) ->
      pp_symbol fmt "exec";
      pp_print_expr fmt body;
      (match globals with
       | None -> ()
       | Some globals ->
           pp_char fmt ' ';
           pp_symbol fmt "in";
           pp_print_expr fmt globals;
           match locals with
           | None -> ()
           | Some locals ->
               pp_char fmt ' ';
               pp_symbol fmt ",";
               pp_print_expr fmt locals)

  | Global (names, _) ->
      pp_symbol fmt "global";
      pp_separated fmt (pp_string fmt) names
           
  | Expr (value, _) ->
      pp_print_expr fmt value

  | Pass (_) -> pp_string fmt "pass"
  | Break (_) -> pp_string fmt "break"
  | Continue (_) -> pp_string fmt "continue"

and pp_print_body fmt = function
  | [] -> ()
  | [s] -> pp_print_stmt fmt s
  | s :: ss ->
      pp_print_stmt fmt s;
      pp_newline fmt;
      pp_print_body fmt ss

and pp_print_suite fmt body =
  pp_open fmt;
  pp_newline fmt;
  pp_print_body fmt body;
  pp_close fmt

and pp_print_orelse fmt = function
  | [] -> ()
  | orelse ->
    pp_newline fmt;
    pp_string fmt "else";
    pp_colon fmt;
    pp_print_suite fmt orelse

and pp_print_if_orelse fmt = function
  | [] -> ()
  | [If (test, body, orelse, _)] ->
      pp_newline fmt;
      pp_symbol fmt "elif";
      pp_print_expr fmt test;
      pp_colon fmt;
      pp_print_suite fmt body;
      pp_print_if_orelse fmt orelse
  | orelse ->
      pp_print_orelse fmt orelse

and pp_print_expr fmt = function
  | BoolOp (op, values, _) ->
      pp_print_expr_list
        ~paren:true
        ~delim:(" " ^ string_of_boolop op ^ " ")
        fmt values

  | BinOp (left, op, right, _) ->
      pp_print_expr fmt left;
      pp_char fmt ' ';
      pp_string fmt (string_of_operator op);
      pp_char fmt ' ';
      pp_print_expr fmt right

  | UnaryOp (op, operand, _) ->
      pp_string fmt
        (match op with
         | Invert -> "~"
         | Not    -> "not "
         | UAdd   -> "+"
         | USub   -> "-");
      pp_print_expr fmt operand

  | Lambda (args, body, _) ->
      pp_symbol fmt "lambda";
      pp_print_arguments ~paren:false fmt args;
      pp_colon fmt;
      pp_char fmt ' ';
      pp_print_expr fmt body

  | IfExp (test, body, orelse, _) ->
      pp_print_expr fmt body;
      pp_char fmt ' ';
      pp_symbol fmt "if";
      pp_print_expr fmt test;
      pp_char fmt ' ';
      pp_symbol fmt "else";
      pp_print_expr fmt orelse

  | Dict (keys, values, _) ->
      pp_char fmt '{';
      pp_open fmt;
      List.iter2
        (fun k v ->
           pp_char fmt ' ';
           pp_print_expr fmt k;
           pp_colon fmt;
           pp_char fmt ' ';
           pp_print_expr fmt v;
           pp_char fmt ',';
           pp_cut fmt)
        keys values;
      pp_close fmt;
      pp_string fmt " }"

  | ListComp (elt, generators, _) ->
      pp_char fmt '[';
      pp_open fmt;
      pp_print_expr fmt elt;
      pp_print_comprehension_list fmt generators;
      pp_close fmt;
      pp_char fmt ']'

  | GeneratorExp (elt, generators, _) ->
      pp_char fmt '(';
      pp_open fmt;
      pp_print_expr fmt elt;
      pp_print_comprehension_list fmt generators;
      pp_close fmt;
      pp_char fmt ')'

  | Yield (None, _) ->
      pp_string fmt "yield"
  | Yield (Some value, _) ->
      pp_symbol fmt "yield";
      pp_print_expr fmt value

  | Compare (left, ops, comparators, _) ->
      pp_print_expr fmt left;
      List.iter2
        (fun op comparator ->
           pp_char fmt ' ';
           pp_string fmt (string_of_cmpop op);
           pp_char fmt ' ';
           pp_print_expr fmt comparator)
        ops comparators

  | Call (func, args, keywords, starargs, kwargs, _) ->
      let comma =
        let delim = ref "" in
          fun fmt ->
            pp_string fmt !delim;
            delim := ", "
      in
        pp_print_expr fmt func;
        pp_char fmt '(';
        List.iter
          (fun arg ->
             comma fmt;
             pp_print_expr fmt arg)
          args;
        List.iter
          (fun (arg, value) ->
             comma fmt;
             pp_string fmt arg;
             pp_char fmt '=';
             pp_print_expr fmt value)
          keywords;
        (match starargs with
         | None -> ()
         | Some starargs ->
             comma fmt;
             pp_char fmt '*';
             pp_print_expr fmt starargs);
        (match kwargs with
         | None -> ()
         | Some kwargs ->
             comma fmt;
             pp_string fmt "**";
             pp_print_expr fmt kwargs);
        pp_char fmt ')'

  | Repr (value, _) ->
      pp_string fmt "repr(";
      pp_print_expr fmt value;
      pp_char fmt ')'

  | Num (Int i, _)
  | Num (LongInt i, _) ->
      pp_int fmt i
  | Num (Float f, _) ->
      pp_float fmt f
  | Num (Imag im, _) ->
      pp_string fmt im

  | Str (s, _) ->
      (* TODO *)
      pp_char fmt '"';
      pp_string fmt (String.escaped s);
      pp_char fmt '"'

  | Attribute (value, attr, _, _) ->
      pp_print_expr fmt value;
      pp_char fmt '.';
      pp_string fmt attr

  | Subscript (expr, slice, _, _) ->
      pp_print_expr fmt expr;
      pp_char fmt '[';
      pp_print_slice fmt slice;
      pp_char fmt ']'

  | Name (id, _, _) ->
      pp_string fmt id

  | List (elts, _, _) ->
      pp_char fmt '[';
      pp_print_expr_list ~paren:false fmt elts;
      pp_char fmt ']'

  | Tuple (elts, _, _) ->
      pp_print_expr_list fmt elts

and pp_print_expr_list ?(paren=true) ?(delim=", ") fmt expr_list =
  if paren then
    pp_char fmt '(';
  pp_open fmt;
  pp_separated ~delim fmt (pp_print_expr fmt) expr_list;
  pp_close fmt;
  if paren then
    pp_char fmt ')'

and pp_print_decorator fmt decorator =
  pp_char fmt '@';
  pp_print_expr fmt decorator;
  pp_newline fmt

and pp_print_decorator_list fmt decorator_list =
  List.iter (pp_print_decorator fmt) decorator_list

and pp_print_slice fmt = function
  | Ellipsis ->
      pp_string fmt "..."
  | Slice (lower, upper, step) ->
      (match lower with
       | None -> ()
       | Some lower ->
           pp_print_expr fmt lower);
      pp_char fmt ':';
      (match upper with
       | None -> ()
       | Some upper ->
           pp_print_expr fmt upper);
      (match step with
       | None -> ()
       | Some step ->
           pp_char fmt ':';
           pp_print_expr fmt step)
  | ExtSlice dims ->
      pp_separated fmt (pp_print_slice fmt) dims
  | Index expr ->
      pp_print_expr fmt expr

and pp_print_comprehension fmt (expr, iter, ifs) =
  pp_symbol fmt "for";
  pp_print_expr fmt expr;
  pp_string fmt " in ";
  pp_print_expr fmt iter;
  List.iter
    (fun if_ ->
       pp_string fmt " if ";
       pp_print_expr fmt if_)
    ifs
  
and pp_print_comprehension_list fmt comprehension_list =
  List.iter
    (fun comprehension ->
       pp_char fmt ' ';
       pp_print_comprehension fmt comprehension)
    comprehension_list

and pp_print_excepthandler fmt = function
  | ExceptHandler (typ, name, body, _) ->
      pp_newline fmt;
      pp_string fmt "except";
      (match typ with
       | None -> ()
       | Some typ ->
           pp_char fmt ' ';
           pp_print_expr fmt typ;
           match name with
           | None -> ()
           | Some name ->
               pp_string fmt " as ";
               pp_print_expr fmt name);
      pp_colon fmt;
      pp_print_suite fmt body

and pp_print_excepthandler_list fmt excepthandler_list =
  List.iter (pp_print_excepthandler fmt) excepthandler_list

and pp_print_arguments ?(paren=true) fmt (args, varargs, kwargs, defaults) =
  let arg_len = List.length args in
  let def_len = List.length defaults in
  let args, keywords = List.partition_at (arg_len - def_len) args in
  let comma =
    let delim = ref "" in
      fun fmt ->
        pp_string fmt !delim;
        delim := ", "
  in
    if paren then
      pp_char fmt '(';
    List.iter
      (fun arg ->
         comma fmt;
         pp_print_expr fmt arg)
      args;
    List.iter2
      (fun keyword def ->
         comma fmt;
         pp_print_expr fmt keyword;
         pp_char fmt '=';
         pp_print_expr fmt def)
      keywords defaults;
    (match varargs with
     | None -> ()
     | Some varargs ->
         comma fmt;
         pp_char fmt '*';
         pp_string fmt varargs);
    (match kwargs with
     | None -> ()
     | Some kwargs ->
         comma fmt;
         pp_string fmt "**";
         pp_string fmt kwargs);
    if paren then
      pp_char fmt ')'

and pp_print_alias fmt = function
  | (name, None) ->
      pp_string fmt name
  | (name, Some asname) ->
      pp_symbol fmt name;
      pp_symbol fmt "as";
      pp_symbol fmt asname

and pp_print_alias_list fmt alias_list =
  pp_separated fmt (pp_print_alias fmt) alias_list

let print_mod = pp_print_mod std_formatter
let print_stmt = pp_print_stmt std_formatter
let print_expr = pp_print_stmt std_formatter
