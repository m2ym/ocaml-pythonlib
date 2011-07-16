(*
 * The full abstract grammar is available at:
 *   http://docs.python.org/library/ast.html#abstract-grammar
 *)

type identifier = string

and 'a modl =
  | Module of 'a stmt list (* body *)
  | Interactive of 'a stmt list (* body *)
  | Expression of 'a expr (* body *)

  | Suite of 'a stmt list (* body *)

and 'a stmt =
  | FunctionDef of identifier (* name *) * 'a arguments (* args *) * 'a stmt list (* body *) * 'a expr list (* decorator_list *) * 'a
  | ClassDef of identifier (* name *) * 'a expr list (* bases *) * 'a stmt list (* body *) * 'a expr list (* decorator_list *) * 'a
  | Return of 'a expr option (* value *) * 'a

  | Delete of 'a expr list (* targets *) * 'a
  | Assign of 'a expr list (* targets *) * 'a expr (* value *) * 'a
  | AugAssign of 'a expr (* target *) * operator (* op *) * 'a expr (* value *) * 'a

  | Print of 'a expr option (* dest *) * 'a expr list (* values *) * bool (* nl *) * 'a

  | For of 'a expr (* target *) * 'a expr (* iter *) * 'a stmt list (* body *) * 'a stmt list (* orelse *) * 'a
  | While of 'a expr (* test *) * 'a stmt list (* body *) * 'a stmt list (* orelse *) * 'a
  | If of 'a expr (* test *) * 'a stmt list (* body *) * 'a stmt list (* orelse *) * 'a
  | With of 'a expr (* context_expr *) * 'a expr option (* optional_vars *) * 'a stmt list (* body *) * 'a

  | Raise of 'a expr option (* type *) * 'a expr option (* inst *) * 'a expr option (* tback *) * 'a
  | TryExcept of 'a stmt list (* body *) * 'a excepthandler list (* handlers *) * 'a stmt list (* orelse *) * 'a
  | TryFinally of 'a stmt list (* body *) * 'a stmt list (* finalbody *) * 'a
  | Assert of 'a expr (* test *) * 'a expr option (* msg *) * 'a

  | Import of alias list (* names *) * 'a
  | ImportFrom of identifier (* module *) * alias list (* names *) * int option (* level *) * 'a

  | Exec of 'a expr (* body *) * 'a expr option (* globals *) * 'a expr option (* locals *) * 'a

  | Global of identifier list (* names *) * 'a
  | Expr of 'a expr (* value *) * 'a
  | Pass of 'a
  | Break of 'a
  | Continue of 'a

and 'a expr =
  | BoolOp of boolop (* op *) * 'a expr list (* values *) * 'a
  | BinOp of 'a expr (* left *) * operator (* op *) * 'a expr (* right *) * 'a
  | UnaryOp of unaryop (* op *) * 'a expr (* operand *) * 'a
  | Lambda of 'a arguments (* args *) * 'a expr (* body *) * 'a
  | IfExp of 'a expr (* test *) * 'a expr (* body *) * 'a expr (* orelse *) * 'a
  | Dict of 'a expr list (* keys *) * 'a expr list (* values *) * 'a
  | ListComp of 'a expr (* elt *) * 'a comprehension list (* generators *) * 'a
  | GeneratorExp of 'a expr (* elt *) * 'a comprehension list (* generators *) * 'a
  | Yield of 'a expr option (* value *) * 'a
  | Compare of 'a expr (* left *) * cmpop list (* ops *) * 'a expr list (* comparators *) * 'a
  | Call of 'a expr (* func *) * 'a expr list (* args *) * 'a keyword list (* keywords *) * 'a expr option (* starargs *) * 'a expr option (* kwargs *) * 'a
  | Repr of 'a expr (* value *) * 'a
  | Num of number (* n *) * 'a
  | Str of string (* s *) * 'a

  | Attribute of 'a expr (* value *) * identifier (* attr *) * expr_context (* ctx *) * 'a
  | Subscript of 'a expr (* value *) * 'a slice (* slice *) * expr_context (* ctx *) * 'a
  | Name of identifier (* id *) * expr_context (* ctx *) * 'a
  | List of 'a expr list (* elts *) * expr_context (* ctx *) * 'a
  | Tuple of 'a expr list (* elts *) * expr_context (* ctx *) * 'a

(* AugLoad and AugStore are not used *)
and expr_context = Load | Store | Del | AugLoad | AugStore | Param

and 'a slice =
  | Ellipsis
  | Slice of 'a expr option (* lower *) * 'a expr option (* upper *) * 'a expr option (* step *)
  | ExtSlice of 'a slice list (* dims *)
  | Index of 'a expr (* value *)

and boolop = And | Or

and operator = Add | Sub | Mult | Div | Mod | Pow | LShift
               | RShift | BitOr | BitXor | BitAnd | FloorDiv

and unaryop = Invert | Not | UAdd | USub

and cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn

and 'a comprehension = 'a expr (* target *) * 'a expr (* iter *) * 'a expr list (* ifs *)

and 'a excepthandler = ExceptHandler of 'a expr option (* type *) * 'a expr option (* name *) * 'a stmt list (* body *) * 'a

and 'a arguments = 'a expr list (* args *) * identifier option (* varargs *) * identifier option (* kwargs *) * 'a expr list (* defaults *)

and 'a keyword = identifier (* arg *) * 'a expr (* value *)

and alias = identifier (* name *) * identifier option (* asname *)

and number =
  | Int of int
  | LongInt of int
  | Float of float
  | Imag of string

let name_of_mod = function
  | Module _      -> "Module"
  | Interactive _ -> "Interactive"
  | Expression _  -> "Expression"
  | Suite _       -> "Suite"

and name_of_stmt = function
  | FunctionDef _ -> "FunctionDef"
  | ClassDef _    -> "ClassDef"
  | Return _      -> "Return"
  | Delete _      -> "Delete"
  | Assign _      -> "Assign"
  | AugAssign _   -> "AugAssign"
  | Print _       -> "Print"
  | For _         -> "For"
  | While _       -> "While"
  | If _          -> "If"
  | With _        -> "With"
  | Raise _       -> "Raise"
  | TryExcept _   -> "TryExcept"
  | TryFinally _  -> "TryFinally"
  | Assert _      -> "Assert"
  | Import _      -> "Import"
  | ImportFrom _  -> "ImportFrom"
  | Exec _        -> "Exec"
  | Global _      -> "Global"
  | Expr _        -> "Expr"
  | Pass _        -> "Pass"
  | Break _       -> "Break"
  | Continue _    -> "Continue"

and name_of_expr = function
  | BoolOp _       -> "BoolOp"
  | BinOp _        -> "BinOp"
  | UnaryOp _      -> "UnaryOp"
  | Lambda _       -> "Lambda"
  | IfExp _        -> "IfExp"
  | Dict _         -> "Dict"
  | ListComp _     -> "ListComp"
  | GeneratorExp _ -> "GeneratorExp"
  | Yield _        -> "Yield"
  | Compare _      -> "Compare"
  | Call _         -> "Call"
  | Repr _         -> "Repr"
  | Num _          -> "Num"
  | Str _          -> "Str"
  | Attribute _    -> "Attribute"
  | Subscript _    -> "Subscript"
  | Name _         -> "Name"
  | List _         -> "List"
  | Tuple _        -> "Tuple"

and name_of_expr_context = function
  | Load        -> "Load"
  | Store       -> "Store"
  | Del         -> "Del"
  | AugLoad     -> "AugLoad"
  | AugStore    -> "AugStore"
  | Param       -> "Param"

and name_of_slice = function
  | Ellipsis    -> "Ellipsis"
  | Slice _     -> "Slice"
  | ExtSlice _  -> "ExtSlice"
  | Index _     -> "Index"

and name_of_boolop = function
  | And -> "And"
  | Or  -> "Or"

and name_of_operator = function
  | Add         -> "Add"
  | Sub         -> "Sub"
  | Mult        -> "Mult"
  | Div         -> "Div"
  | Mod         -> "Mod"
  | Pow         -> "Pow"
  | LShift      -> "LShift"
  | RShift      -> "RShift"
  | BitOr       -> "BitOr"
  | BitXor      -> "BitXor"
  | BitAnd      -> "BitAnd"
  | FloorDiv    -> "FloorDiv"

and name_of_unaryop = function
  | Invert      -> "Insert"
  | Not         -> "Not"
  | UAdd        -> "UAdd"
  | USub        -> "USub"

and name_of_cmpop = function
  | Eq          -> "Eq"
  | NotEq       -> "NotEq"
  | Lt          -> "Lt"
  | LtE         -> "LtE"
  | Gt          -> "Gt"
  | GtE         -> "GtE"
  | Is          -> "Is"
  | IsNot       -> "IsNot"
  | In          -> "In"
  | NotIn       -> "NotIn"

and name_of_excepthandler = function
  | ExceptHandler _ -> "ExceptHandler"

and name_of_number = function
  | Int _       -> "Int"
  | LongInt _   -> "LongInt"
  | Float _     -> "Float"
  | Imag _      -> "Imag"

let annot_of_stmt = function
  | FunctionDef (_, _, _, _, a)
  | ClassDef (_, _, _, _, a)
  | Return (_, a)
  | Delete (_, a)
  | Assign (_, _, a)
  | AugAssign (_, _, _, a)
  | Print (_, _, _, a)
  | For (_, _, _, _, a)
  | While (_, _, _, a)
  | If (_, _, _, a)
  | With (_, _, _, a)
  | Raise (_, _, _, a)
  | TryExcept (_, _, _, a)
  | TryFinally (_, _, a)
  | Assert (_, _, a)
  | Import (_, a)
  | ImportFrom (_, _, _, a)
  | Exec (_, _, _, a)
  | Global (_, a)
  | Expr (_, a)
  | Pass (a)
  | Break (a)
  | Continue (a)
    -> a

and annot_of_expr = function
  | BoolOp (_, _, a)
  | BinOp (_, _, _, a)
  | UnaryOp (_, _, a)
  | Lambda (_, _, a)
  | IfExp (_, _, _, a)
  | Dict (_, _, a)
  | ListComp (_, _, a)
  | GeneratorExp (_, _, a)
  | Yield (_, a)
  | Compare (_, _, _, a)
  | Call (_, _, _, _, _, a)
  | Repr (_, a)
  | Num (_, a)
  | Str (_, a)
  | Attribute (_, _, _, a)
  | Subscript (_, _, _, a)
  | Name (_, _, a)
  | List (_, _, a)
  | Tuple (_, _, a)
    -> a

and annot_of_excepthandler = function
  | ExceptHandler (_, _, _, a) -> a

let context_of_expr = function
  | Attribute (_, _, ctx, _) -> Some ctx
  | Subscript (_, _, ctx, _) -> Some ctx
  | Name (_, ctx, _)         -> Some ctx
  | List (_, ctx, _)         -> Some ctx
  | Tuple (_, ctx, _)        -> Some ctx
  | _                        -> None

let string_of_boolop = function
  | And -> "and"
  | Or  -> "or"

let string_of_operator = function
  | Add         -> "+"
  | Sub         -> "-"
  | Mult        -> "*"
  | Div         -> "/"
  | Mod         -> "%"
  | Pow         -> "**"
  | LShift      -> "<<"
  | RShift      -> ">>"
  | BitOr       -> "|"
  | BitXor      -> "^"
  | BitAnd      -> "&"
  | FloorDiv    -> "//"

let string_of_unaryop = function
  | Invert -> "~"
  | Not    -> "not"
  | UAdd   -> "+"
  | USub   -> "-"

let string_of_cmpop = function
  | Eq    -> "=="
  | NotEq -> "!="
  | Lt    -> "<"
  | LtE   -> "<="
  | Gt    -> ">"
  | GtE   -> ">="
  | Is    -> "is"
  | IsNot -> "is not"
  | In    -> "in"
  | NotIn -> "not in"

let string_of_number = function
  | Int (n)      -> string_of_int n
  | LongInt (n)  -> (string_of_int n) ^ "L"
  | Float (n)    -> (string_of_float n)
  | Imag (n)     -> n

module type Annot = sig
  type t
  val of_pos : Lexing.position -> t
end

module Pos : Annot = struct
  type t = Lexing.position
  let of_pos pos = pos
end
