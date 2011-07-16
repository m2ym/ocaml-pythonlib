open Format
open Ast
open Utils

let mk_sep sep =
  let first = ref true in
    fun fmt ->
      if !first then
        first := false
      else
        fprintf fmt sep

let pp_char = pp_print_char
let pp_string = pp_print_string
let pp_int = pp_print_int
let pp_float = pp_print_float

let pp_empty fmt _ = ()

let pp_paren pp fmt = fprintf fmt "(%a)" pp

let pp_opt pp fmt = function
  | None -> ()
  | Some x -> pp fmt x

let rec pp_list ?(sep=", ") pp fmt = function
  | [] -> ()
  | [x] -> pp fmt x
  | x :: xs ->
      fprintf fmt "%a%s%a"
        pp x sep (pp_list pp) xs

let rec pp_block pp fmt = function
  | [] -> ()
  | [x] -> pp fmt x
  | x :: xs ->
      fprintf fmt "%a@\n%a"
        pp x (pp_block pp) xs

let rec pp_mod fmt = function
  | Module body
  | Interactive body
  | Suite body ->
      pp_block pp_stmt fmt body
  | Expression expr ->
      pp_expr fmt expr

and pp_stmt fmt = function
  | FunctionDef (name, args, body, decorator_list, _) ->
      fprintf fmt "%a@[<4>def %s(%a):@\n%a@]"
        (pp_block pp_decorator) decorator_list
        name
        pp_args args
        pp_suite body

  | ClassDef (name, bases, body, decorator_list, _) ->
      fprintf fmt "%a@[<4>class %s%a:@\n%a@]"
        (pp_block pp_decorator) decorator_list
        name
        (if bases <> []
         then pp_paren_expr_list
         else pp_empty) bases
        pp_suite body

  | Return (None, _) ->
      fprintf fmt "return"
  | Return (Some value, _) ->
      fprintf fmt "return %a" pp_expr value

  | Delete (targets, _) ->
      fprintf fmt "del %a" pp_expr_list targets

  | Assign (targets, value, _) ->
      fprintf fmt "%a = %a"
        pp_expr_list targets
        pp_expr value

  | AugAssign (target, op, value, _) ->
      fprintf fmt "%a %s= %a"
        pp_expr target
        (string_of_operator op)
        pp_expr value

  | Print (None, values, nl, _) ->
      fprintf fmt "print %a" pp_expr_list values;
      if not nl then
        pp_char fmt ','
  | Print (Some dest, [], nl, _) ->
      fprintf fmt "print >> %a" pp_expr dest;
      if not nl then
        pp_char fmt ','
  | Print (Some dest, values, nl, _) ->
      fprintf fmt "print >> %a, %a"
        pp_expr dest
        pp_expr_list values;
      if not nl then
        pp_char fmt ','

  | For (target, iter, body, orelse, _) ->
      fprintf fmt "@[<4>for %a in %a:@\n%a@]%a"
        pp_expr target
        pp_expr iter
        pp_suite body
        pp_orelse orelse

  | While (test, body, orelse, _) ->
      fprintf fmt "@[<4>while %a:@\n%a@]%a"
        pp_expr test
        pp_suite body
        pp_orelse orelse

  | If (test, body, orelse, _) ->
      fprintf fmt "@[<4>if %a:@\n%a@]%a"
        pp_expr test
        pp_suite body
        pp_if_orelse orelse

  | With (context_expr, None, body, _) ->
      fprintf fmt "@[<4>with %a:@\n%a@]"
        pp_expr context_expr
        pp_suite body
  | With (context_expr, Some optional_vars, body, _) ->
      fprintf fmt "@[<4>with %a as %a:@\n%a@]"
        pp_expr context_expr
        pp_expr optional_vars
        pp_suite body

  | Raise (None, _, _, _) ->
      fprintf fmt "raise"
  | Raise (Some typ, None, _, _) ->
      fprintf fmt "raise %a" pp_expr typ
  | Raise (Some typ, Some inst, None, _) ->
      fprintf fmt "raise %a, %a" pp_expr typ pp_expr inst
  | Raise (Some typ, Some inst, Some tback, _) ->
      fprintf fmt "raise %a, %a, %a"
        pp_expr typ
        pp_expr inst
        pp_expr tback

  | TryExcept (body, handlers, orelse, _) ->
      fprintf fmt "@[<4>try:@\n%a@]@\n%a%a"
        pp_suite body
        (pp_block pp_excepthandler) handlers
        pp_orelse orelse

  | TryFinally ([TryExcept _ as try_except], finalbody, _) ->
      fprintf fmt "%a@[<4>finally:@\n%a@]"
        pp_stmt try_except
        pp_suite finalbody
  | TryFinally (body, finalbody, _) ->
      fprintf fmt "@[<4>try:@\n%a@]@\n@[<4>finally:@\n%a@]"
        pp_suite body
        pp_suite finalbody

  | Assert (test, None, _) ->
      fprintf fmt "assert %a" pp_expr test
  | Assert (test, Some msg, _) ->
      fprintf fmt "assert %a, %a" pp_expr test pp_expr msg

  | Import (names, _) ->
      fprintf fmt "import %a" pp_alias_list names

  | ImportFrom (modl, names, level, _) ->
      fprintf fmt "from %s%s import %a"
        (String.make
           (match level with
            | None -> 0
            | Some n -> n)
           ' ')
        modl
        pp_alias_list names

  | Exec (body, None, _, _) ->
      fprintf fmt "exec %a" pp_expr body
  | Exec (body, Some globals, None, _) ->
      fprintf fmt "exec %a in %a" pp_expr body pp_expr globals
  | Exec (body, Some globals, Some locals, _) ->
      fprintf fmt "exec %a in %a, %a"
        pp_expr body
        pp_expr globals
        pp_expr locals

  | Global (names, _) ->
      fprintf fmt "global %a" (pp_list pp_string) names

  | Expr (value, _) ->
      pp_expr fmt value

  | Pass (_) -> pp_string fmt "pass"
  | Break (_) -> pp_string fmt "break"
  | Continue (_) -> pp_string fmt "continue"

and pp_suite fmt = pp_block pp_stmt fmt

and pp_orelse fmt = function
  | [] -> ()
  | orelse ->
      fprintf fmt "@\n@[<4>else:@\n%a@]" pp_suite orelse

and pp_if_orelse fmt = function
  | [] -> ()
  | [If (test, body, orelse, _)] ->
      fprintf fmt "@\n@[<4>elif %a:@\n%a@]%a"
        pp_expr test
        pp_suite body
        pp_if_orelse orelse
  | orelse ->
      pp_orelse fmt orelse

and pp_expr fmt = function
  | BoolOp (op, values, _) ->
      let sep = Printf.sprintf " %s " (string_of_boolop op) in
        pp_paren (pp_list ~sep pp_expr) fmt values

  | BinOp (left, op, right, _) ->
      fprintf fmt "%a %s %a"
        pp_expr left
        (string_of_operator op)
        pp_expr right

  | UnaryOp (op, operand, _) ->
      pp_string fmt
        (match op with
         | Invert -> "~"
         | Not    -> "not "
         | UAdd   -> "+"
         | USub   -> "-");
      pp_expr fmt operand

  | Lambda (args, body, _) ->
      fprintf fmt "lambda %a: %a" pp_args args pp_expr body

  | IfExp (test, body, orelse, _) ->
      fprintf fmt "%a if %a else %a"
        pp_expr test
        pp_expr body
        pp_expr orelse

  | Dict (keys, values, _) ->
      fprintf fmt "{@[@ ";
      List.iter2
        (fun k v -> fprintf fmt "%a: %a,@ " pp_expr k pp_expr v)
        keys values;
      fprintf fmt "@]}"

  | ListComp (elt, generators, _) ->
      fprintf fmt "[@[%a %a@]]"
        pp_expr elt
        (pp_list ~sep:" " pp_comp) generators

  | GeneratorExp (elt, generators, _) ->
      fprintf fmt "(@[%a %a@])"
        pp_expr elt
        (pp_list ~sep:" " pp_comp) generators

  | Yield (None, _) ->
      fprintf fmt "yield"
  | Yield (Some value, _) ->
      fprintf fmt "yield %a" pp_expr value

  | Compare (left, ops, comparators, _) ->
      pp_expr fmt left;
      List.iter2
        (fun op comparator ->
           fprintf fmt " %s %a"
             (string_of_cmpop op)
             pp_expr comparator)
        ops comparators

  | Call (func, args, keywords, starargs, kwargs, _) ->
      let comma = mk_sep ", " in
        fprintf fmt "%a(" pp_expr func;
        List.iter (fprintf fmt "%t%a" comma pp_expr) args;
        List.iter
          (fun (arg, value) ->
             fprintf fmt "%t%s=%a" comma arg pp_expr value)
          keywords;
        pp_opt (fun fmt -> fprintf fmt "%t*%a" comma pp_expr) fmt starargs;
        pp_opt (fun fmt -> fprintf fmt "%t**%a" comma pp_expr) fmt kwargs;
        pp_char fmt ')'

  | Repr (value, _) ->
      fprintf fmt "repr(%a)" pp_expr value

  | Num (Int i, _)
  | Num (LongInt i, _) ->
      pp_int fmt i
  | Num (Float f, _) ->
      pp_float fmt f
  | Num (Imag im, _) ->
      pp_string fmt im

  | Str (s, _) ->
      (* TODO *)
      fprintf fmt "\"%s\"" (String.escaped s)

  | Attribute (value, attr, _, _) ->
      fprintf fmt "%a.%s" pp_expr value attr

  | Subscript (expr, slice, _, _) ->
      fprintf fmt "%a[@[%a@]]" pp_expr expr pp_slice slice

  | Name (id, _, _) ->
      pp_string fmt id

  | List (elts, _, _) ->
      fprintf fmt "[@[%a@]]" pp_expr_list elts

  | Tuple (elts, _, _) ->
      pp_paren_expr_list fmt elts

and pp_expr_list fmt = pp_list pp_expr fmt
and pp_paren_expr_list fmt = pp_paren pp_expr_list fmt

and pp_decorator fmt = fprintf fmt "@<0>@[<4>@@%a@]@\n" pp_expr

and pp_slice fmt = function
  | Ellipsis ->
      pp_string fmt "..."
  | Slice (lower, upper, step) ->
      pp_opt pp_expr fmt lower;
      pp_char fmt ':';
      pp_opt pp_expr fmt upper;
      pp_opt (fun fmt -> fprintf fmt ":%a" pp_expr) fmt step
  | ExtSlice dims ->
      pp_list pp_slice fmt dims
  | Index expr ->
      pp_expr fmt expr

and pp_comp fmt (expr, iter, ifs) =
  fprintf fmt "for %a in %a" pp_expr expr pp_expr iter;
  List.iter (fprintf fmt " if %a" pp_expr) ifs

and pp_excepthandler fmt = function
  | ExceptHandler (None, _, body, _) ->
      fprintf fmt "@[<4>except:@\n%a@]" pp_suite body
  | ExceptHandler (Some typ, None, body, _) ->
      fprintf fmt "@[<4>except %a:@\n%a@]"
        pp_expr typ
        pp_suite body
  | ExceptHandler (Some typ, Some name, body, _) ->
      fprintf fmt "@[<4>except %a as %a:@\n%a@]"
        pp_expr typ
        pp_expr name
        pp_suite body

and pp_args fmt (args, varargs, kwargs, defaults) =
  let arg_len = List.length args in
  let def_len = List.length defaults in
  let args, keywords = List.partition_at (arg_len - def_len) args in
  let comma = mk_sep ", " in
    fprintf fmt "@<0>@[<4>";
    List.iter (fprintf fmt "%t%a" comma pp_expr) args;
    List.iter2
      (fun key def ->
         fprintf fmt "%t%a=%a"
           comma
           pp_expr key
           pp_expr def)
      keywords defaults;
    pp_opt (fun fmt -> fprintf fmt "%t*%s" comma) fmt varargs;
    pp_opt (fun fmt -> fprintf fmt "%t**%s" comma) fmt kwargs;
    fprintf fmt "@]"

and pp_alias fmt = function
  | (name, None) ->
      pp_string fmt name
  | (name, Some asname) ->
      fprintf fmt "%s as %s" name asname

and pp_alias_list fmt = pp_list pp_alias fmt

let pp_print_mod fmt modl = pp_mod fmt modl
let pp_print_stmt fmt stmt = pp_stmt fmt stmt
let pp_print_expr fmt expr = pp_expr fmt expr
let print_mod modl = pp_print_mod std_formatter modl
let print_stmt stmt = pp_print_stmt std_formatter stmt
let print_expr expr = pp_print_expr std_formatter expr
