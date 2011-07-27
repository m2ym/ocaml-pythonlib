open Format
open Ast

let pp_char = pp_print_char
let pp_string = pp_print_string
let pp_int = pp_print_int
let pp_float = pp_print_float
let pp_bool = pp_print_bool

let rec pp_list ?(bracket=true) pp fmt list =
  let rec loop fmt = function
    | [] -> ()
    | [x] -> pp fmt x
    | x :: xs ->
        fprintf fmt "%a, %a"
          pp x loop xs
  in
    if bracket then
      fprintf fmt "[%a]" loop list
    else
      loop fmt list

let pp_opt pp fmt = function
  | None -> pp_string fmt "None"
  | Some x -> pp fmt x

let rec pp_identifier fmt id =
  fprintf fmt "'%s'" id

and pp_num fmt num =
  pp_string fmt (string_of_number num)

and pp_str fmt str =
  (* TODO *)
  let escape_char d fmt =
    function
    | '\r' -> pp_string fmt "\\r"
    | '\n' -> pp_string fmt "\\n"
    | '\\' -> pp_string fmt "\\\\"
    | '"' when d -> pp_string fmt "\\\""
    | '\'' when not d -> pp_string fmt "\\'"
    | c -> pp_char fmt c
  in
  let escape_str d fmt s =
    String.iter (escape_char d fmt) s
  in
    if String.contains str '\'' && not (String.contains str '"') then
      fprintf fmt "\"%a\"" (escape_str true) str
    else
      fprintf fmt "\'%a\'" (escape_str false) str

and pp_expr_context fmt ctx =
  pp_node fmt (name_of_expr_context ctx) []

and pp_slice fmt = function
  | Ellipsis ->
      pp_node fmt "Ellipsis" []
  | Slice (lower, upper, step) ->
      pp_node fmt "Slice"
        ["lower", `Expr_opt lower;
         "upper", `Expr_opt upper;
         "step", `Expr_opt step]
  | ExtSlice (dims) ->
      pp_node fmt "ExtSlice" ["dims", `Slice_list dims]
  | Index (value) ->
      pp_node fmt "Index" ["value", `Expr value]

and pp_boolop fmt op =
  pp_node fmt (name_of_boolop op) []

and pp_operator fmt op =
  pp_node fmt (name_of_operator op) []

and pp_unaryop fmt op =
  pp_node fmt (name_of_unaryop op) []

and pp_cmpop fmt op =
  pp_node fmt (name_of_cmpop op) []

and pp_comprehension fmt (target, iter, ifs) =
  pp_node fmt "comprehension"
    ["target", `Expr target;
     "iter", `Expr iter;
     "ifs", `Expr_list ifs]

and pp_arguments fmt (args, vararg, kwarg, defaults) =
  pp_node fmt "arguments"
    ["args", `Expr_list args;
     "vararg", `Identifier_opt vararg;
     "kwarg", `Identifier_opt kwarg;
     "defaults", `Expr_list defaults]

and pp_excepthandler fmt = function
  | ExceptHandler (typ, name, body, pos) ->
      pp_node fmt "ExceptHandler"
        ["type", `Expr_opt typ;
         "name", `Expr_opt name;
         "body", `Stmt_list body]

and pp_keyword fmt (arg, value) =
  pp_node fmt "keyword"
    ["arg", `Identifier arg;
     "value", `Expr value]

and pp_alias fmt (name, asname) =
  pp_node fmt "alias"
    ["name", `Identifier name;
     "asname", `Identifier_opt asname]

and pp_field fmt (label, field) =
  fprintf fmt "%s=" label;
  match field with
  | `Int i -> pp_int fmt i
  | `Bool b -> pp_bool fmt b
  | `Identifier id -> pp_identifier fmt id
  | `Identifier_opt id -> pp_opt pp_identifier fmt id
  | `Identifier_list ids -> pp_list pp_identifier fmt ids
  | `Num num -> pp_num fmt num
  | `Str str -> pp_str fmt str
  | `Stmt stmt -> pp_stmt fmt stmt
  | `Stmt_list stmts -> pp_list pp_stmt fmt stmts
  | `Expr expr -> pp_expr fmt expr
  | `Expr_opt expr -> pp_opt pp_expr fmt expr
  | `Expr_list exprs -> pp_list pp_expr fmt exprs
  | `Expr_context ctx -> pp_expr_context fmt ctx
  | `Slice slice -> pp_slice fmt slice
  | `Slice_list slices -> pp_list pp_slice fmt slices
  | `Boolop op -> pp_boolop fmt op
  | `Operator op -> pp_operator fmt op
  | `Unaryop op -> pp_unaryop fmt op
  | `Cmpop op -> pp_cmpop fmt op
  | `Cmpop_list ops -> pp_list pp_cmpop fmt ops
  | `Comprehension_list comps -> pp_list pp_comprehension fmt comps
  | `Arguments args -> pp_arguments fmt args
  | `Excepthandler_list handlers -> pp_list pp_excepthandler fmt handlers
  | `Keyword_list keywords -> pp_list pp_keyword fmt keywords
  | `Alias_list aliases -> pp_list pp_alias fmt aliases

and pp_node fmt label fields =
  fprintf fmt "%s(%a)"
    label (pp_list ~bracket:false pp_field) fields

and pp_mod fmt = function
  | Module (body, _) ->
      pp_node fmt "Module" ["body", `Stmt_list body]

  | Interactive (body, _) ->
      pp_node fmt "Interactive" ["body", `Stmt_list body]

  | Expression (body, _) ->
      pp_node fmt "Expression" ["body", `Expr body]

  | Suite (body, _) ->
      pp_node fmt "Suite" ["body", `Stmt_list body]

and pp_stmt fmt = function
  | FunctionDef (name, args, body, decorator_list, _) ->
      pp_node fmt "FunctionDef"
        ["name", `Identifier name;
         "args", `Arguments args;
         "body", `Stmt_list body;
         "decorator_list", `Expr_list decorator_list]

  | ClassDef (name, bases, body, decorator_list, _) ->
      pp_node fmt "ClassDef"
        ["name", `Identifier name;
         "bases", `Expr_list bases;
         "body", `Stmt_list body;
         "decorator_list", `Expr_list decorator_list]

  | Return (value, _) ->
      pp_node fmt "Return" ["value", `Expr_opt value]

  | Delete (targets, _) ->
      pp_node fmt "Delete" ["targets", `Expr_list targets]

  | Assign (targets, value, _) ->
      pp_node fmt "Assign"
        ["targets", `Expr_list targets;
         "value", `Expr value]

  | AugAssign (target, op, value, _) ->
      pp_node fmt "AugAssign"
        ["target", `Expr target;
         "op", `Operator op;
         "value", `Expr value]

  | Print (dest, values, nl, _) ->
      pp_node fmt "Print"
        ["dest", `Expr_opt dest;
         "values", `Expr_list values;
         "nl", `Bool nl]

  | For (target, iter, body, orelse, _) ->
      pp_node fmt "For"
        ["target", `Expr target;
         "iter", `Expr iter;
         "body", `Stmt_list body;
         "orelse", `Stmt_list orelse]

  | While (test, body, orelse, _) ->
      pp_node fmt "While"
        ["test", `Expr test;
         "body", `Stmt_list body;
         "orelse", `Stmt_list orelse]

  | If (test, body, orelse, _) ->
      pp_node fmt "If"
        ["test", `Expr test;
         "body", `Stmt_list body;
         "orelse", `Stmt_list orelse]

  | With (context_expr, optional_vars, body, _) ->
      pp_node fmt "With"
        ["context_expr", `Expr context_expr;
         "optional_vars", `Expr_opt optional_vars;
         "body", `Stmt_list body]

  | Raise (typ, inst, tback, _) ->
      pp_node fmt "Raise"
        ["type", `Expr_opt typ;
         "inst", `Expr_opt inst;
         "tback", `Expr_opt tback]

  | TryExcept (body, handlers, orelse, _) ->
      pp_node fmt "TryExcept"
        ["body", `Stmt_list body;
         "handlers", `Excepthandler_list handlers;
         "orelse", `Stmt_list orelse]
  | TryFinally (body, finalbody, _) ->
      pp_node fmt "TryFinally"
        ["body", `Stmt_list body;
         "finalbody", `Stmt_list finalbody]

  | Assert (test, msg, _) ->
      pp_node fmt "Assert"
        ["test", `Expr test;
         "msg", `Expr_opt msg]

  | Import (names, _) ->
      pp_node fmt "Import" ["names", `Alias_list names]

  | ImportFrom (modul, names, level, _) ->
      (match level with
       | Some l ->
           pp_node fmt "ImportFrom"
             ["module", `Identifier modul;
              "names", `Alias_list names;
              "level", `Int l]
       | None -> failwith "Unreachable")

  | Exec (body, globals, locals, _) ->
      pp_node fmt "Exec"
        ["body", `Expr body;
         "globals", `Expr_opt globals;
         "locals", `Expr_opt locals]

  | Global (names, _) ->
      pp_node fmt "Global" ["names", `Identifier_list names]

  | Expr (value, _) ->
      pp_node fmt "Expr" ["value", `Expr value]

  | Pass (_) -> pp_node fmt "Pass" []

  | Break (_) -> pp_node fmt "Break" []

  | Continue (_) -> pp_node fmt "Continue" []

and pp_expr fmt = function
  | BoolOp (op, values, _) ->
      pp_node fmt "BoolOp"
        ["op", `Boolop op;
         "values", `Expr_list values]

  | BinOp (left, op, right, _) ->
      pp_node fmt "BinOp"
        ["left", `Expr left;
         "op", `Operator op;
         "right", `Expr right]

  | UnaryOp (op, operand, _) ->
      pp_node fmt "UnaryOp"
        ["op", `Unaryop op;
         "operand", `Expr operand]

  | Lambda (args, body, _) ->
      pp_node fmt "Lambda"
        ["args", `Arguments args;
         "body", `Expr body]

  | IfExp (test, body, orelse, _) ->
      pp_node fmt "IfExp"
        ["test", `Expr test;
         "body", `Expr body;
         "orelse", `Expr orelse;]

  | Dict (keys, values, _) ->
      pp_node fmt "Dict"
        ["keys", `Expr_list keys;
         "values", `Expr_list values]

  | ListComp (elt, generators, _) ->
      pp_node fmt "ListComp"
        ["elt", `Expr elt;
         "generators", `Comprehension_list generators]

  | GeneratorExp (elt, generators, _) ->
      pp_node fmt "GeneratorExp"
        ["elt", `Expr elt;
         "generators", `Comprehension_list generators]

  | Yield (value, _) ->
      pp_node fmt "Yield" ["value", `Expr_opt value]

  | Compare (left, ops, comparators, _) ->
      pp_node fmt "Compare"
        ["left", `Expr left;
         "ops", `Cmpop_list ops;
         "comparators", `Expr_list comparators]

  | Call (func, args, keywords, starargs, kwargs, _) ->
      pp_node fmt "Call"
        ["func", `Expr func;
         "args", `Expr_list args;
         "keywords", `Keyword_list keywords;
         "starargs", `Expr_opt starargs;
         "kwargs", `Expr_opt kwargs]

  | Repr (value, _) ->
      pp_node fmt "Repr" ["value", `Expr value]

  | Num (n, _) ->
      pp_node fmt "Num" ["n", `Num n]

  | Str (s, _) ->
      pp_node fmt "Str" ["s", `Str s]

  | Attribute (value, attr, ctx, _) ->
      pp_node fmt "Attribute"
        ["value", `Expr value;
         "attr", `Identifier attr;
         "ctx", `Expr_context ctx]

  | Subscript (value, slice, ctx, _) ->
      pp_node fmt "Subscript"
        ["value", `Expr value;
         "slice", `Slice slice;
         "ctx", `Expr_context ctx]

  | Name (id, ctx, _) ->
      pp_node fmt "Name"
        ["id", `Identifier id;
         "ctx", `Expr_context ctx]

  | List (elts, ctx, _) ->
      pp_node fmt "List"
        ["elts", `Expr_list elts;
         "ctx", `Expr_context ctx]

  | Tuple (elts, ctx, _) ->
      pp_node fmt "Tuple"
        ["elts", `Expr_list elts;
         "ctx", `Expr_context ctx]

let pp_print_mod fmt modl = pp_mod fmt modl
let pp_print_stmt fmt stmt = pp_stmt fmt stmt
let pp_print_expr fmt expr = pp_expr fmt expr
let print_mod modl = pp_print_mod std_formatter modl
let print_stmt stmt = pp_print_stmt std_formatter stmt
let print_expr expr = pp_print_expr std_formatter expr

let print_to_string pp node =
  let buf = Buffer.create 4096 in
  let fmt = formatter_of_buffer buf in
    pp fmt node;
    pp_print_flush fmt ();
    Buffer.contents buf

let dump_mod modl = print_to_string pp_print_mod modl
let dump_stmt stmt = print_to_string pp_print_stmt stmt
let dump_expr expr = print_to_string pp_print_expr expr
