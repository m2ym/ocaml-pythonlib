(*
 * The full abstract grammar is available at:
 *   http://docs.python.org/library/ast.html#abstract-grammar
 *)

type pos = Lexing.position

let dummy_pos = Lexing.dummy_pos

type identifier = string

and modl =
  | Module of stmt list (* body *)
  | Interactive of stmt list (* body *)
  | Expression of expr (* body *)

  | Suite of stmt list (* body *)

and stmt =
  | FunctionDef of identifier (* name *) * arguments (* args *) * stmt list (* body *) * expr list (* decorator_list *) * pos
  | ClassDef of identifier (* name *) * expr list (* bases *) * stmt list (* body *) * expr list (* decorator_list *) * pos
  | Return of expr option (* value *) * pos

  | Delete of expr list (* targets *) * pos
  | Assign of expr list (* targets *) * expr (* value *) * pos
  | AugAssign of expr (* target *) * operator (* op *) * expr (* value *) * pos

  | Print of expr option (* dest *) * expr list (* values *) * bool (* nl *) * pos

  | For of expr (* target *) * expr (* iter *) * stmt list (* body *) * stmt list (* orelse *) * pos
  | While of expr (* test *) * stmt list (* body *) * stmt list (* orelse *) * pos
  | If of expr (* test *) * stmt list (* body *) * stmt list (* orelse *) * pos
  | With of expr (* context_expr *) * expr option (* optional_vars *) * stmt list (* body *) * pos

  | Raise of expr option (* type *) * expr option (* inst *) * expr option (* tback *) * pos
  | TryExcept of stmt list (* body *) * excepthandler list (* handlers *) * stmt list (* orelse *) * pos
  | TryFinally of stmt list (* body *) * stmt list (* finalbody *) * pos
  | Assert of expr (* test *) * expr option (* msg *) * pos

  | Import of alias list (* names *) * pos
  | ImportFrom of identifier (* module *) * alias list (* names *) * int option (* level *) * pos

  | Exec of expr (* body *) * expr option (* globals *) * expr option (* locals *) * pos

  | Global of identifier list (* names *) * pos
  | Expr of expr (* value *) * pos
  | Pass of pos
  | Break of pos
  | Continue of pos

and expr =
  | BoolOp of boolop (* op *) * expr list (* values *) * pos
  | BinOp of expr (* left *) * operator (* op *) * expr (* right *) * pos
  | UnaryOp of unaryop (* op *) * expr (* operand *) * pos
  | Lambda of arguments (* args *) * expr (* body *) * pos
  | IfExp of expr (* test *) * expr (* body *) * expr (* orelse *) * pos
  | Dict of expr list (* keys *) * expr list (* values *) * pos
  | ListComp of expr (* elt *) * comprehension list (* generators *) * pos
  | GeneratorExp of expr (* elt *) * comprehension list (* generators *) * pos
  | Yield of expr option (* value *) * pos
  | Compare of expr (* left *) * cmpop list (* ops *) * expr list (* comparators *) * pos
  | Call of expr (* func *) * expr list (* args *) * keyword list (* keywords *) * expr option (* starargs *) * expr option (* kwargs *) * pos
  | Repr of expr (* value *) * pos
  | Num of number (* n *) * pos
  | Str of string (* s *) * pos

  | Attribute of expr (* value *) * identifier (* attr *) * expr_context (* ctx *) * pos
  | Subscript of expr (* value *) * slice (* slice *) * expr_context (* ctx *) * pos
  | Name of identifier (* id *) * expr_context (* ctx *) * pos
  | List of expr list (* elts *) * expr_context (* ctx *) * pos
  | Tuple of expr list (* elts *) * expr_context (* ctx *) * pos

(* AugLoad and AugStore are not used *)
and expr_context = Load | Store | Del | AugLoad | AugStore | Param

and slice =
  | Ellipsis
  | Slice of expr option (* lower *) * expr option (* upper *) * expr option (* step *)
  | ExtSlice of slice list (* dims *)
  | Index of expr (* value *)

and boolop = And | Or

and operator = Add | Sub | Mult | Div | Mod | Pow | LShift
               | RShift | BitOr | BitXor | BitAnd | FloorDiv

and unaryop = Invert | Not | UAdd | USub

and cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn

and comprehension = expr (* target *) * expr (* iter *) * expr list (* ifs *)

and excepthandler = ExceptHandler of expr option (* type *) * expr option (* name *) * stmt list (* body *) * pos

and arguments = expr list (* args *) * identifier option (* varargs *) * identifier option (* kwargs *) * expr list (* defaults *)

and keyword = identifier (* arg *) * expr (* value *)

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

let pos_of_stmt = function
  | FunctionDef (_, _, _, _, pos)
  | ClassDef (_, _, _, _, pos)
  | Return (_, pos)
  | Delete (_, pos)
  | Assign (_, _, pos)
  | AugAssign (_, _, _, pos)
  | Print (_, _, _, pos)
  | For (_, _, _, _, pos)
  | While (_, _, _, pos)
  | If (_, _, _, pos)
  | With (_, _, _, pos)
  | Raise (_, _, _, pos)
  | TryExcept (_, _, _, pos)
  | TryFinally (_, _, pos)
  | Assert (_, _, pos)
  | Import (_, pos)
  | ImportFrom (_, _, _, pos)
  | Exec (_, _, _, pos)
  | Global (_, pos)
  | Expr (_, pos)
  | Pass (pos)
  | Break (pos)
  | Continue (pos)
    -> pos

and pos_of_expr = function
  | BoolOp (_, _, pos)
  | BinOp (_, _, _, pos)
  | UnaryOp (_, _, pos)
  | Lambda (_, _, pos)
  | IfExp (_, _, _, pos)
  | Dict (_, _, pos)
  | ListComp (_, _, pos)
  | GeneratorExp (_, _, pos)
  | Yield (_, pos)
  | Compare (_, _, _, pos)
  | Call (_, _, _, _, _, pos)
  | Repr (_, pos)
  | Num (_, pos)
  | Str (_, pos)
  | Attribute (_, _, _, pos)
  | Subscript (_, _, _, pos)
  | Name (_, _, pos)
  | List (_, _, pos)
  | Tuple (_, _, pos)
      -> pos

and pos_of_excepthandler = function
    | ExceptHandler (_, _, _, pos) -> pos

let context_of_expr = function
  | Attribute (_, _, ctx, _) -> Some ctx
  | Subscript (_, _, ctx, _) -> Some ctx
  | Name (_, ctx, _)         -> Some ctx
  | List (_, ctx, _)         -> Some ctx
  | Tuple (_, ctx, _)        -> Some ctx
  | _                        -> None

let string_of_number = function
  | Int (n)      -> string_of_int n
  | LongInt (n)  -> (string_of_int n) ^ "L"
  | Float (n)    -> (string_of_float n)
  | Imag (n)     -> n
