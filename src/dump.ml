open Format
open Ast

let pp_char = pp_print_char
let pp_string = pp_print_string
let pp_dump_label = pp_string

let rec pp_dump_list fmt f lst =
  pp_char fmt '[';
  pp_dump_list_contents fmt f lst;
  pp_char fmt ']'

and pp_dump_list_contents fmt f = function
  | [] -> ()
  | [x] -> f x
  | x::xs ->
      f x;
      pp_string fmt ", ";
      pp_dump_list_contents fmt f xs

and pp_dump_opt fmt f = function
  | Some (x) -> f x
  | None -> pp_string fmt "None"

and pp_dump_field_start fmt label =
  pp_string fmt label;
  pp_char fmt '='

and pp_dump_field fmt label f x =
  pp_dump_field_start fmt label;
  f x

and pp_dump_field_opt fmt label f x =
  pp_dump_field_start fmt label;
  pp_dump_opt fmt f x

and pp_dump_field_list fmt label f lst =
  pp_dump_field_start fmt label;
  pp_dump_list fmt f lst

(* dump operation combinators *)
let rec node label fields a fmt =
  pp_dump_label fmt label;
  pp_char fmt '(';
  pp_dump_list_contents fmt (fun f -> f fmt) fields;
  pp_char fmt ')'

and field_identifier label id fmt =
  pp_dump_field fmt label (pp_dump_identifier fmt) id

and field_identifier_opt label id fmt =
  pp_dump_field_opt fmt label (pp_dump_identifier fmt) id

and field_identifier_list label ids fmt =
  pp_dump_field_list fmt label (pp_dump_identifier fmt) ids

and field_num label num fmt =
  pp_dump_field fmt label (pp_dump_num fmt) num

and field_str label str fmt =
  pp_dump_field fmt label (pp_dump_str fmt) str

and field_int label i fmt =
  pp_dump_field fmt label (pp_dump_int fmt) i

and field_bool label b fmt =
  pp_dump_field fmt label (pp_dump_bool fmt) b

and field_stmt_list label stmt_list fmt =
  pp_dump_field_list fmt label (pp_dump_stmt fmt) stmt_list

and field_expr label expr fmt =
  pp_dump_field fmt label (pp_dump_expr fmt) expr

and field_expr_opt label expr fmt =
  pp_dump_field_opt fmt label (pp_dump_expr fmt) expr

and field_expr_list label expr_list fmt =
  pp_dump_field_list fmt label (pp_dump_expr fmt) expr_list

and field_expr_context label ctx fmt =
  pp_dump_field fmt label (pp_dump_expr_context fmt) ctx

and field_slice label slice fmt =
  pp_dump_field fmt label (pp_dump_slice fmt) slice

and field_slice_list label slice_list fmt =
  pp_dump_field_list fmt label (pp_dump_slice fmt) slice_list

and field_boolop label op fmt =
  pp_dump_field fmt label (pp_dump_boolop fmt) op

and field_operator label op fmt =
  pp_dump_field fmt label (pp_dump_operator fmt) op

and field_unaryop label op fmt =
  pp_dump_field fmt label (pp_dump_unaryop fmt) op

and field_cmpop_list label ops fmt =
  pp_dump_field_list fmt label (pp_dump_cmpop fmt) ops

and field_comprehension_list label comps fmt =
  pp_dump_field_list fmt label (pp_dump_comprehension fmt) comps

and field_arguments label args fmt =
  pp_dump_field fmt label (pp_dump_arguments fmt) args

and field_excepthandler_list label handlers fmt =
  pp_dump_field_list fmt label (pp_dump_excepthandler fmt) handlers

and field_keyword_list label keyword_list fmt =
  pp_dump_field_list fmt label (pp_dump_keyword fmt) keyword_list

and field_alias_list label alias_list fmt =
  pp_dump_field_list fmt label (pp_dump_alias fmt) alias_list

(* implementation *)
and pp_dump_stmt fmt = function
  | FunctionDef (name, args, body, decorator_list, a) ->
      node "FunctionDef"
        [field_identifier"name" name;
         field_arguments "args" args;
         field_stmt_list "body" body;
         field_expr_list "decorator_list" decorator_list]
        a fmt
  | ClassDef (name, bases, body, decorator_list, a) ->
      node "ClassDef"
        [field_identifier"name" name;
         field_expr_list "bases" bases;
         field_stmt_list "body" body;
         field_expr_list "decorator_list" decorator_list]
        a fmt
  | Return (value, a) ->
      node "Return" [field_expr_opt "value" value] a fmt
  | Delete (targets, a) ->
      node "Delete" [field_expr_list "targets" targets] a fmt
  | Assign (targets, value, a) ->
      node "Assign"
        [field_expr_list "targets" targets;
         field_expr "value" value]
        a fmt
  | AugAssign (target, op, value, a) ->
      node "AugAssign"
        [field_expr "target" target;
         field_operator "op" op;
         field_expr "value" value]
        a fmt
  | Print (dest, values, nl, a) ->
      node "Print"
        [field_expr_opt "dest" dest;
         field_expr_list "values" values;
         field_bool "nl" nl]
        a fmt
  | For (target, iter, body, orelse, a) ->
      node "For"
        [field_expr "target" target;
         field_expr "iter" iter;
         field_stmt_list "body" body;
         field_stmt_list "orelse" orelse]
        a fmt
  | While (test, body, orelse, a) ->
      node "While"
        [field_expr "test" test;
         field_stmt_list "body" body;
         field_stmt_list "orelse" orelse]
        a fmt
  | If (test, body, orelse, a) ->
      node "If"
        [field_expr "test" test;
         field_stmt_list "body" body;
         field_stmt_list "orelse" orelse]
        a fmt
  | With (context_expr, optional_vars, body, a) ->
      node "With"
        [field_expr "context_expr" context_expr;
         field_expr_opt "optional_vars" optional_vars;
         field_stmt_list "body" body]
        a fmt
  | Raise (typ, inst, tback, a) ->
      node "Raise"
        [field_expr_opt "type" typ;
         field_expr_opt "inst" inst;
         field_expr_opt "tback" tback]
        a fmt
  | TryExcept (body, handlers, orelse, a) ->
      node "TryExcept"
        [field_stmt_list "body" body;
         field_excepthandler_list "handlers" handlers;
         field_stmt_list "orelse" orelse]
        a fmt
  | TryFinally (body, finalbody, a) ->
      node "TryFinally"
        [field_stmt_list "body" body;
         field_stmt_list "finalbody" finalbody]
        a fmt
  | Assert (test, msg, a) ->
      node "Assert"
        [field_expr "test" test;
         field_expr_opt "msg" msg]
        a fmt
  | Import (names, a) ->
      node "Import" [field_alias_list "names" names] a fmt
  | ImportFrom (modul, names, level, a) ->
      (match level with
       | Some l ->
           node "ImportFrom"
             [field_identifier "module" modul;
              field_alias_list "names" names;
              field_int "level" l]
             a fmt
       | None -> failwith "Unreachable")
  | Exec (body, globals, locals, a) ->
      node "Exec"
        [field_expr "body" body;
         field_expr_opt "globals" globals;
         field_expr_opt "locals" locals]
        a fmt
  | Global (names, a) ->
      node "Global" [field_identifier_list "names" names] a fmt
  | Expr (value, a) ->
      node "Expr" [field_expr "value" value] a fmt
  | Pass (a) -> node "Pass" [] a fmt
  | Break (a) -> node "Break" [] a fmt
  | Continue (a) -> node "Continue" [] a fmt

and pp_dump_expr fmt = function
  | BoolOp (op, values, a) ->
      node "BoolOp"
        [field_boolop "op" op;
         field_expr_list "values" values]
        a fmt
  | BinOp (left, op, right, a) ->
      node "BinOp"
        [field_expr "left" left;
         field_operator "op" op;
         field_expr "right" right]
        a fmt
  | UnaryOp (op, operand, a) ->
      node "UnaryOp"
        [field_unaryop "op" op;
         field_expr "operand" operand]
        a fmt
  | Lambda (args, body, a) ->
      node "Lambda"
        [field_arguments "args" args;
         field_expr "body" body]
        a fmt
  | IfExp (test, body, orelse, a) ->
      node "IfExp"
        [field_expr "test" test;
         field_expr "body" body;
         field_expr "orelse" orelse;]
        a fmt
  | Dict (keys, values, a) ->
      node "Dict"
        [field_expr_list "keys" keys;
         field_expr_list "values" values]
        a fmt
  | ListComp (elt, generators, a) ->
      node "ListComp"
        [field_expr "elt" elt;
         field_comprehension_list "generators" generators]
        a fmt
  | GeneratorExp (elt, generators, a) ->
      node "GeneratorExp"
        [field_expr "elt" elt;
         field_comprehension_list "generators" generators]
        a fmt
  | Yield (value, a) ->
      node "Yield" [field_expr_opt "value" value] a fmt
  | Compare (left, ops, comparators, a) ->
      node "Compare"
        [field_expr "left" left;
         field_cmpop_list "ops" ops;
         field_expr_list "comparators" comparators]
        a fmt
  | Call (func, args, keywords, starargs, kwargs, a) ->
      node "Call"
        [field_expr "func" func;
         field_expr_list "args" args;
         field_keyword_list "keywords" keywords;
         field_expr_opt "starargs" starargs;
         field_expr_opt "kwargs" kwargs]
        a fmt
  | Repr (value, a) ->
      node "Repr" [field_expr "value" value] a fmt
  | Num (n, a) ->
      node "Num" [field_num "n" n] a fmt
  | Str (s, a) ->
      node "Str" [field_str "s" s] a fmt
  | Attribute (value, attr, ctx, a) ->
      node "Attribute"
        [field_expr "value" value;
         field_identifier "attr" attr;
         field_expr_context "ctx" ctx]
        a fmt
  | Subscript (value, slice, ctx, a) ->
      node "Subscript"
        [field_expr "value" value;
         field_slice "slice" slice;
         field_expr_context "ctx" ctx]
        a fmt
  | Name (id, ctx, a) ->
      node "Name"
        [field_identifier "id" id;
         field_expr_context "ctx" ctx]
        a fmt
  | List (elts, ctx, a) ->
      node "List"
        [field_expr_list "elts" elts;
         field_expr_context "ctx" ctx]
        a fmt
  | Tuple (elts, ctx, a) ->
      node "Tuple"
        [field_expr_list "elts" elts;
         field_expr_context "ctx" ctx]
        a fmt

and pp_dump_mod fmt = function
  | Module (body) ->
      node "Module" [field_stmt_list "body" body] dummy_pos fmt
  | Interactive (body) ->
      node "Interactive" [field_stmt_list "body" body] dummy_pos fmt
  | Expression (body) ->
      node "Expression" [field_expr "body" body] dummy_pos fmt
  | Suite (body) ->
      node "Suite" [field_stmt_list "body" body] dummy_pos fmt

and pp_dump_identifier fmt id =
  pp_char fmt '\'';
  pp_string fmt id;
  pp_char fmt '\'';

and pp_dump_num fmt n = pp_string fmt (string_of_number n)

and pp_dump_bool fmt b =
  pp_string fmt (if b then "True" else "False")

and pp_dump_int fmt i = pp_string fmt (string_of_int i)

and pp_dump_str fmt s =
  if String.contains s '\'' && not (String.contains s '"') then begin
    pp_char fmt '"';
    String.iter (function
                   | '\r' -> pp_string fmt "\\r"
                   | '\n' -> pp_string fmt "\\n"
                   | '\\' -> pp_string fmt "\\\\"
                   | '"' -> pp_string fmt "\\\""
                   | c -> pp_char fmt c)
      s;
    pp_char fmt '"'
  end else begin
    pp_char fmt '\'';
    String.iter (function
                   | '\r' -> pp_string fmt "\\r"
                   | '\n' -> pp_string fmt "\\n"
                   | '\\' -> pp_string fmt "\\\\"
                   | '\'' -> pp_string fmt "\\'"
                   | c -> pp_char fmt c)
      s;
    pp_char fmt '\''
  end

and pp_dump_expr_context fmt ctx =
  node (name_of_expr_context ctx) [] dummy_pos fmt;

and pp_dump_slice fmt = function
  | Ellipsis ->
      node "Ellipsis" [] dummy_pos fmt
  | Slice (lower, upper, step) ->
      node "Slice"
        [field_expr_opt "lower" lower;
         field_expr_opt "upper" upper;
         field_expr_opt "step" step]
        dummy_pos fmt
  | ExtSlice (dims) ->
      node "ExtSlice" [field_slice_list "dims" dims] dummy_pos fmt
  | Index (value) ->
      node "Index" [field_expr "value" value] dummy_pos fmt

and pp_dump_boolop fmt op =
  node (name_of_boolop op) [] dummy_pos fmt

and pp_dump_operator fmt op =
  node (name_of_operator op) [] dummy_pos fmt

and pp_dump_unaryop fmt op =
  node (name_of_unaryop op) [] dummy_pos fmt

and pp_dump_cmpop fmt op =
  node (name_of_cmpop op) [] dummy_pos fmt

and pp_dump_comprehension fmt = function
  | target, iter, ifs ->
      node "comprehension"
        [field_expr "target" target;
         field_expr "iter" iter;
         field_expr_list "ifs" ifs]
        dummy_pos fmt

and pp_dump_arguments fmt = function
  | args, vararg, kwarg, defaults ->
      node "arguments"
        [field_expr_list "args" args;
         field_identifier_opt "vararg" vararg;
         field_identifier_opt "kwarg" kwarg;
         field_expr_list "defaults" defaults]
        dummy_pos fmt

and pp_dump_excepthandler fmt = function
  | ExceptHandler (typ, name, body, pos) ->
      node "ExceptHandler"
        [field_expr_opt "type" typ;
         field_expr_opt "name" name;
         field_stmt_list "body" body]
        dummy_pos fmt

and pp_dump_keyword fmt keyword =
  node "keyword"
    [field_identifier "arg" (fst keyword);
     field_expr "value" (snd keyword)]
    dummy_pos fmt

and pp_dump_alias fmt alias =
  node "alias"
    [field_identifier "name" (fst alias);
     field_identifier_opt "asname" (snd alias)]
    dummy_pos fmt

let dump_mod modl =
  let fmt = std_formatter in
    pp_dump_mod fmt modl;
    pp_print_flush fmt ()

let dump_expr expr =
  let fmt = std_formatter in
    pp_dump_expr fmt expr;
    pp_print_flush fmt ()
