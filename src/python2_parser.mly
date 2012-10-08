%{
  open Token
  module Make (A : Ast.Annot) = struct
    open Ast

    let annot = A.of_pos

    type ('a,'b) either = Left of 'a | Right of 'b

    let singleton e = Left e
    and tuple e = Right [e]
    and cons e = function
      | Left e' -> Right (e::[e'])
      | Right l -> Right (e::l)
    and tuple_expr = function
      | Left e -> e
      | Right l -> Tuple (l, Load, annot Lexing.dummy_pos)
    and to_list = function
      | Left e -> [e]
      | Right l -> l

    let rec set_expr_ctx ctx = function
      | Attribute (value, attr, _, a) ->
          Attribute (value, attr, ctx, a)
      | Subscript (value, slice, _, a) ->
          Subscript (value, slice, ctx, a)
      | Name (id, _, a) ->
          Name (id, ctx, a)
      | List (elts, _, a) ->
          List (List.map (set_expr_ctx ctx) elts, ctx, a)
      | Tuple (elts, _, a) ->
          Tuple (List.map (set_expr_ctx ctx) elts, ctx, a)
      | e -> e

    let expr_store = set_expr_ctx Store
    and expr_del = set_expr_ctx Del

    let tuple_expr_store l =
      let e = tuple_expr l in
        match context_of_expr e with
        | Some Param -> e
        | _ -> expr_store e
%}

/* literals */
%token <string * Lexing.position>   NAME
%token <int * Lexing.position>      INT
%token <int * Lexing.position>      LONGINT
%token <float * Lexing.position>    FLOAT
%token <string * Lexing.position>   IMAG
%token <string * Lexing.position>   STR

/* layout */
%token INDENT
%token DEDENT
%token NEWLINE

/* keywords */
%token <Lexing.position> AND
%token <Lexing.position> AS
%token <Lexing.position> ASSERT
%token <Lexing.position> BREAK
%token <Lexing.position> CLASS
%token <Lexing.position> CONTINUE
%token <Lexing.position> DEF
%token <Lexing.position> DEL
%token <Lexing.position> ELIF
%token <Lexing.position> ELSE
%token <Lexing.position> EXCEPT
%token <Lexing.position> EXEC
%token <Lexing.position> FINALLY
%token <Lexing.position> FOR
%token <Lexing.position> FROM
%token <Lexing.position> GLOBAL
%token <Lexing.position> IF
%token <Lexing.position> IMPORT
%token <Lexing.position> IN
%token <Lexing.position> IS
%token <Lexing.position> LAMBDA
%token <Lexing.position> NOT
%token <Lexing.position> OR
%token <Lexing.position> PASS
%token <Lexing.position> PRINT
%token <Lexing.position> RAISE
%token <Lexing.position> RETURN
%token <Lexing.position> TRY
%token <Lexing.position> WHILE
%token <Lexing.position> WITH
%token <Lexing.position> YIELD

/* symbols */
%token <Lexing.position> ADD            /* + */
%token <Lexing.position> SUB            /* - */
%token <Lexing.position> MULT           /* * */
%token <Lexing.position> DIV            /* / */
%token <Lexing.position> MOD            /* % */
%token <Lexing.position> POW            /* ** */
%token <Lexing.position> FDIV           /* // */
%token <Lexing.position> BITOR          /* | */
%token <Lexing.position> BITAND         /* & */
%token <Lexing.position> BITXOR         /* ^ */
%token <Lexing.position> BITNOT         /* ~ */
%token <Lexing.position> LSHIFT         /* << */
%token <Lexing.position> RSHIFT         /* >> */

%token <Lexing.position> EQ             /* = */
%token <Lexing.position> ADDEQ          /* += */
%token <Lexing.position> SUBEQ          /* -= */
%token <Lexing.position> MULTEQ         /* *= */
%token <Lexing.position> DIVEQ          /* /= */
%token <Lexing.position> MODEQ          /* %= */
%token <Lexing.position> POWEQ          /* **= */
%token <Lexing.position> FDIVEQ         /* //= */
%token <Lexing.position> ANDEQ          /* &= */
%token <Lexing.position> OREQ           /* |= */
%token <Lexing.position> XOREQ          /* ^= */
%token <Lexing.position> LSHEQ          /* <<= */
%token <Lexing.position> RSHEQ          /* >>= */

%token <Lexing.position> EQUAL          /* == */
%token <Lexing.position> NOTEQ          /* !=, <> */
%token <Lexing.position> LT             /* < */
%token <Lexing.position> GT             /* > */
%token <Lexing.position> LEQ            /* <= */
%token <Lexing.position> GEQ            /* >= */

%token <Lexing.position> LPAREN         /* ( */
%token <Lexing.position> RPAREN         /* ) */
%token <Lexing.position> LBRACK         /* [ */
%token <Lexing.position> RBRACK         /* ] */
%token <Lexing.position> LBRACE         /* { */
%token <Lexing.position> RBRACE         /* } */
%token <Lexing.position> COLON          /* : */
%token <Lexing.position> SEMICOL        /* ; */
%token <Lexing.position> DOT            /* . */
%token <Lexing.position> COMMA          /* , */
%token <Lexing.position> BACKQUOTE      /* ` */
%token <Lexing.position> AT             /* @ */

/* eof */
%token ENDMARKER

%start file_input
%type <A.t Ast.modl> file_input

%start expr
%type <A.t Ast.expr> expr

%%

file_input:
  | nl_stmt_list ENDMARKER
      { Module ($1, annot Lexing.dummy_pos) }

decorator:
  | AT decorator_expr NEWLINE { $2 }

decorator_name:
  | atom_name { $1 }
  | atom_name DOT NAME { Attribute ($1, fst $3, Load, annot (snd $3)) }

decorator_expr:
  | decorator_name { $1 }
  | decorator_name LPAREN RPAREN { Call ($1, [], [], None, None, annot $2) }
  | decorator_name LPAREN arglist RPAREN
      { match $3 with
        | args, keywords, starargs, kwargs ->
            Call ($1, args, keywords, starargs, kwargs, annot $2) }

decorators:
  | { [] }
  | decorator decorators { $1::$2 }

funcdef:
  | decorators DEF name parameters COLON suite
      { FunctionDef ($3, $4, $6, $1, annot $2) }

parameters:
  | LPAREN varargslist RPAREN { $2 }

varargslist:
  | { [], None, None, [] }
  | fpdef { [$1], None, None, [] }
  | fpdef COMMA varargslist
      { match $3 with
        | args, varargs, kwargs, defaults  ->
            $1::args, varargs, kwargs, defaults }
  | fpdef EQ test
      {
        (* TODO check default arguments come after
           variable arguments with semantic analysis. *)
        [$1], None, None, [$3] }
  | fpdef EQ test COMMA varargslist
      { match $5 with
        | args, varargs, kwargs, defaults  ->
            $1::args, varargs, kwargs, $3::defaults }
  | fpvarargs
      { [], fst $1, snd $1, [] }

fpdef:
  | NAME { Name (fst $1, Param, annot (snd $1)) }
  | LPAREN fplist RPAREN { tuple_expr_store $2 }

fplist:
  | fpdef { singleton $1 }
  | fpdef COMMA { tuple $1 }
  | fpdef COMMA fplist { cons $1 $3 }

fpvarargs:
  | MULT name { Some $2, None }
  | MULT name COMMA fpkwargs { Some $2, $4 }
  | fpkwargs { None, $1 }

fpkwargs:
  | POW name { Some $2 }

stmt:
  | simple_stmt { $1 }
  | compound_stmt { [$1] }

stmt_list:
  | { [] }
  | stmt stmt_list { $1 @ $2 }

nl_stmt_list:
  | { [] }
  | NEWLINE nl_stmt_list { $2 }
  | stmt nl_stmt_list { $1 @ $2 }

simple_stmt:
  | small_stmt NEWLINE { [$1] }
  | small_stmt SEMICOL NEWLINE { [$1] }
  | small_stmt SEMICOL simple_stmt { $1::$3 }

small_stmt:
  | expr_stmt   { $1 }
  | print_stmt  { $1 }
  | del_stmt    { $1 }
  | pass_stmt   { $1 }
  | flow_stmt   { $1 }
  | import_stmt { $1 }
  | global_stmt { $1 }
  | exec_stmt   { $1 }
  | assert_stmt { $1 }

expr_stmt:
  | expr_stmt_lhs EQ expr_stmt_rhs_list { Assign ($1::(fst $3), snd $3, annot $2) }
  | expr_stmt_lhs augassign expr_stmt_rhs { AugAssign ($1, fst $2, $3, annot (snd $2)) }
  | testlist_expr { Expr ($1, annot_of_expr $1) }
      
expr_stmt_lhs:
  | testlist { tuple_expr_store $1 }

expr_stmt_rhs:
  | yield_expr { $1 }
  | testlist_expr { $1 }

expr_stmt_rhs_list:
  | expr_stmt_rhs { [], $1 }
  | expr_stmt_lhs EQ expr_stmt_rhs_list { $1::(fst $3), snd $3 }

augassign:
  | ADDEQ   { Add, $1 }
  | SUBEQ   { Sub, $1 }
  | MULTEQ  { Mult, $1 }
  | DIVEQ   { Div, $1 }
  | POWEQ   { Pow, $1 }
  | MODEQ   { Mod, $1 }
  | LSHEQ   { LShift, $1 }
  | RSHEQ   { RShift, $1 }
  | OREQ    { BitOr, $1 }
  | XOREQ   { BitXor, $1 }
  | ANDEQ   { BitAnd, $1 }
  | FDIVEQ  { FloorDiv, $1 }

print_stmt:
  | PRINT
      { (* TODO "from __future__ import print_function" *)
        Print (None, [], true, annot $1) }
  | PRINT test print_testlist { Print (None, $2::(fst $3), snd $3, annot $1) }
  | PRINT RSHIFT test { Print (Some $3, [], true, annot $1) }
  | PRINT RSHIFT test COMMA test print_testlist { Print (Some $3, $5::(fst $6), snd $6, annot $1) }

print_testlist:
  | { [], true }
  | COMMA test COMMA { [$2], false }
  | COMMA test print_testlist { $2::(fst $3), snd $3 }

del_stmt:
  | DEL exprlist { Delete (List.map expr_del (to_list $2), annot $1) }

pass_stmt:
  | PASS { Pass (annot $1) }

flow_stmt:
  | break_stmt    { $1 }
  | continue_stmt { $1 }
  | return_stmt   { $1 }
  | raise_stmt    { $1 }
  | yield_stmt    { $1 }

break_stmt:
  | BREAK { Break (annot $1) }

continue_stmt:
  | CONTINUE { Continue (annot $1) }

return_stmt:
  | RETURN { Return (None, annot $1) }
  | RETURN testlist { Return (Some (tuple_expr $2), annot $1) }

yield_stmt:
  | yield_expr { Expr ($1, annot_of_expr $1) }

raise_stmt:
  | RAISE { Raise (None, None, None, annot $1) }
  | RAISE test { Raise (Some $2, None, None, annot $1) }
  | RAISE test COMMA test { Raise (Some $2, Some $4, None, annot $1) }
  | RAISE test COMMA test COMMA test { Raise (Some $2, Some $4, Some $6, annot $1) }

import_stmt:
  | import_name { $1 }
  | import_from { $1 }

import_name:
  | IMPORT dotted_as_names { Import ($2, annot $1) }

import_from:
  | FROM name_and_level IMPORT MULT
      { ImportFrom (fst $2, ["*", None], snd $2, annot $1) }
  | FROM name_and_level IMPORT LPAREN import_as_names RPAREN
      { ImportFrom (fst $2, $5, snd $2, annot $1) }
  | FROM name_and_level IMPORT import_as_names
      { ImportFrom (fst $2, $4, snd $2, annot $1) }

name_and_level:
  | dotted_name { $1, Some 0 }
  | dot_level dotted_name { $2, Some $1 }
  | DOT dot_level { "", Some (1 + $2) }

dot_level:
  | { 0 }
  | DOT dot_level { 1 + $2 }

import_as_name:
  | name { $1, None }
  | name AS name { $1, Some $3 }

dotted_as_name:
  | dotted_name { $1, None }
  | dotted_name AS name { $1, Some $3 }

import_as_names:
  | import_as_name { [$1] }
  | import_as_name COMMA { [$1] }
  | import_as_name COMMA import_as_names { $1::$3 }

dotted_as_names:
  | dotted_as_name { [$1] }
  | dotted_as_name COMMA dotted_as_names { $1::$3 }

dotted_name:
  | name { $1 }
  | name DOT dotted_name { $1 ^ "." ^ $3 }

global_stmt:
  | GLOBAL name_list { Global ($2, annot $1) }

name_list:
  | name { [$1] }
  | name COMMA name_list { $1::$3 }

exec_stmt:
  | EXEC expr { Exec ($2, None, None, annot $1) }
  | EXEC expr IN test { Exec ($2, Some $4, None, annot $1) }
  | EXEC expr IN test COMMA test { Exec ($2, Some $4, Some $6, annot $1) }

assert_stmt:
  | ASSERT test { Assert ($2, None, annot $1) }
  | ASSERT test COMMA test { Assert ($2, Some $4, annot $1) }

compound_stmt:
  | if_stmt     { $1 }
  | while_stmt  { $1 }
  | for_stmt    { $1 }
  | try_stmt    { $1 }
  | with_stmt   { $1 }
  | funcdef     { $1 }
  | classdef    { $1 }

if_stmt:
  | IF test COLON suite elif_stmt_list { If ($2, $4, $5, annot $1) }

elif_stmt_list:
  | { [] }
  | ELIF test COLON suite elif_stmt_list { [If ($2, $4, $5, annot $1)] }
  | ELSE COLON suite { $3 }

while_stmt:
  | WHILE test COLON suite { While ($2, $4, [], annot $1) }
  | WHILE test COLON suite ELSE COLON suite { While ($2, $4, $7, annot $1) }

for_stmt:
  | FOR exprlist IN testlist COLON suite
      { For (tuple_expr_store $2, tuple_expr $4, $6, [], annot $1) }
  | FOR exprlist IN testlist COLON suite ELSE COLON suite
      { For (tuple_expr_store $2, tuple_expr $4, $6, $9, annot $1) }

try_stmt:
  | TRY COLON suite excepthandler_list
      { TryExcept ($3, $4, [], annot $1) }
  | TRY COLON suite excepthandler_list ELSE COLON suite
      { TryExcept ($3, $4, $7, annot $1) }
  | TRY COLON suite excepthandler_list ELSE COLON suite FINALLY COLON suite
      { TryFinally ([TryExcept ($3, $4, $7, annot $1)], $10, annot $8) }
  | TRY COLON suite excepthandler_list FINALLY COLON suite
      { TryFinally ([TryExcept ($3, $4, [], annot $1)], $7, annot $5) }
  | TRY COLON suite FINALLY COLON suite
      { TryFinally ($3, $6, annot $1) }

excepthandler:
  | EXCEPT COLON suite { ExceptHandler (None, None, $3, annot $1) }
  | EXCEPT test COLON suite { ExceptHandler (Some $2, None, $4, annot $1) }
  | EXCEPT test AS test COLON suite { ExceptHandler (Some $2, Some $4, $6, annot $1) }
  | EXCEPT test COMMA test COLON suite { ExceptHandler (Some $2, Some (expr_store $4), $6, annot $1) }

excepthandler_list:
  | excepthandler { [$1] }
  | excepthandler excepthandler_list { $1::$2 }

with_stmt:
  | WITH test COLON suite { With ($2, None, $4, annot $1) }
  | WITH test AS expr COLON suite { With ($2, Some $4, $6, annot $1) }

suite:
  | simple_stmt { $1 }
  | NEWLINE INDENT stmt_list DEDENT { $3 }

testlist_safe:
  | old_test { singleton $1 }
  | old_test COMMA { tuple $1 }
  | old_test COMMA testlist_safe { cons $1 $3 }

old_test:
  | or_test { $1 }
  | old_lambdadef { $1 }

old_lambdadef:
  | LAMBDA varargslist COLON old_test { Lambda ($2, $4, annot $1) }

test:
  | or_test { $1 }
  | or_test IF or_test ELSE test { IfExp ($3, $1, $5, annot $2) }
  | lambdadef { $1 }

test_opt:
  | { None }
  | test { Some $1 }

or_test:
  | and_test { $1 }
  | and_test OR and_test_list { BoolOp (Or, $1::$3, annot $2) }

and_test:
  | not_test { $1 }
  | not_test AND not_test_list { BoolOp (And, $1::$3, annot $2) }

and_test_list:
  | and_test { [$1] }
  | and_test OR and_test_list { $1::$3 }

not_test:
  | NOT not_test { UnaryOp (Not, $2, annot $1) }
  | comparison { $1 }

not_test_list:
  | not_test { [$1] }
  | not_test AND not_test_list { $1::$3 }

comparison:
  | expr { $1 }
  | expr comp_op comparison_list { Compare ($1, (fst $2)::(fst $3), snd $3, annot (snd $2)) }

comparison_list:
  | expr { [], [$1] }
  | expr comp_op comparison_list { (fst $2)::(fst $3), $1::(snd $3) }

comp_op:
  | EQUAL   { Eq, $1 }
  | NOTEQ   { NotEq, $1 }
  | LT      { Lt, $1 }
  | LEQ     { LtE, $1 }
  | GT      { Gt, $1 }
  | GEQ     { GtE, $1 }
  | IS      { Is, $1 }
  | IS NOT  { IsNot, $1 }
  | IN      { In, $1 }
  | NOT IN  { NotIn, $1 }

expr:
  | xor_expr { $1 }
  | expr BITOR xor_expr{ BinOp ($1, BitOr, $3, annot $2) }

xor_expr:
  | and_expr { $1 }
  | xor_expr BITXOR and_expr { BinOp ($1, BitXor, $3, annot $2) }

and_expr:
  | shift_expr { $1 }
  | shift_expr BITAND and_expr { BinOp ($1, BitAnd, $3, annot $2) }

shift_expr:
  | arith_expr { $1 }
  | shift_expr LSHIFT arith_expr { BinOp ($1, LShift, $3, annot $2) }
  | shift_expr RSHIFT arith_expr { BinOp ($1, RShift, $3, annot $2) }

arith_expr:
  | term { $1 }
  | arith_expr ADD term { BinOp ($1, Add, $3, annot $2) }
  | arith_expr SUB term { BinOp ($1, Sub, $3, annot $2) }

term:
  | factor { $1 }
  | factor term_op term { BinOp ($1, fst $2, $3, annot (snd $2)) }

term_op:
  | MULT    { Mult, $1 }
  | DIV     { Div, $1 }
  | MOD     { Mod, $1 }
  | FDIV    { FloorDiv, $1 }

factor:
  | ADD factor { UnaryOp (UAdd, $2, annot $1) }
  | SUB factor
      { (* CPython converts
             UnaryOp (op=Sub (), operand=Num (n=x))
           to
             Num (n=-x)
           if possible. *)
        match $2 with
        | Num (Int (n), a)        -> Num (Int (-n), a)
        | Num (LongInt (n), a)    -> Num (LongInt (-n), a)
        | Num (Float (n), a)      -> Num (Float (-.n), a)
        | Num (Imag (n), a)       -> Num (Imag ("-" ^ n), a)
        | _                       -> UnaryOp (USub, $2, annot $1) }
  | BITNOT factor { UnaryOp (Invert, $2, annot $1) }
  | power { $1 }

power:
  | atom_trailer { $1 }
  | atom_trailer POW factor { BinOp ($1, Pow, $3, annot $2) }

atom_trailer:
  | atom { $1 }
  | atom_trailer LPAREN RPAREN { Call ($1, [], [], None, None, annot $2) }
  | atom_trailer LPAREN arglist RPAREN
      { match $3 with
        | args, keywords, starargs, kwargs ->
            Call ($1, args, keywords, starargs, kwargs, annot $2) }
  | atom_trailer LBRACK subscriptlist RBRACK
      { match $3 with
          (* TODO test* => Index (Tuple (elts)) *)
        | [s] -> Subscript ($1, s, Load, annot $2)
        | l -> Subscript ($1, ExtSlice (l), Load, annot $2) }
  | atom_trailer DOT NAME { Attribute ($1, fst $3, Load, annot (snd $3)) }

atom:
  | atom_tuple  { $1 }
  | atom_list   { $1 }
  | atom_dict   { $1 }
  | atom_repr   { $1 }
  | atom_name   { $1 }
  | INT         { Num (Int (fst $1), annot (snd $1)) }
  | LONGINT     { Num (LongInt (fst $1), annot (snd $1)) }
  | FLOAT       { Num (Float (fst $1), annot (snd $1)) }
  | IMAG        { Num (Imag (fst $1), annot (snd $1)) }
  | string_list { Str (String.concat "" (fst $1), annot (snd $1)) }

atom_tuple:
  | LPAREN RPAREN { Tuple ([], Load, annot $1) }
  | LPAREN yield_expr RPAREN { $2 }
  | LPAREN test gen_for RPAREN { GeneratorExp ($2, $3, annot $1) }
  | LPAREN testlist RPAREN { tuple_expr $2 }

atom_list:
  | LBRACK RBRACK { List ([], Load, annot $1) }
  | LBRACK test list_for RBRACK { ListComp ($2, $3, annot $1) }
  | LBRACK testlist RBRACK { List (to_list $2, Load, annot $1) }

atom_dict:
  | LBRACE RBRACE { Dict ([], [], annot $1) }
  | LBRACE dictmaker RBRACE { Dict (fst $2, snd $2, annot $1) }

atom_repr:
  | BACKQUOTE testlist1 BACKQUOTE { Repr (tuple_expr $2, annot $1) }

atom_name:
  | NAME { Name (fst $1, Load, annot (snd $1)) }

dictmaker:
  | test COLON test { [$1], [$3] }
  | test COLON test COMMA { [$1], [$3] }
  | test COLON test COMMA dictmaker { $1::(fst $5), $3::(snd $5) }

string_list:
  | STR { ([fst $1], snd $1) }
  | STR string_list { (fst $1::fst $2, snd $1) }

lambdadef:
  | LAMBDA varargslist COLON test { Lambda ($2, $4, annot $1) }

subscriptlist:
  | subscript { [$1] }
  | subscript COMMA { [$1] }
  | subscript COMMA subscriptlist { $1::$3 }

subscript:
  | DOT DOT DOT { Ellipsis }
  | test { Index ($1) }
  | test_opt COLON test_opt { Slice ($1, $3, None) }
  | test_opt COLON test_opt COLON test_opt { Slice ($1, $3, $5) }

exprlist:
  | expr { singleton $1 }
  | expr COMMA { tuple $1 }
  | expr COMMA exprlist { cons $1 $3 }

testlist:
  | test { singleton $1 }
  | test COMMA { tuple $1 }
  | test COMMA testlist { cons $1 $3 }

testlist_expr:
  | testlist { tuple_expr $1 }

classdef:
  | decorators CLASS name COLON suite { ClassDef ($3, [], $5, $1, annot $2) }
  | decorators CLASS name LPAREN RPAREN COLON suite { ClassDef ($3, [], $7, $1, annot $2) }
  | decorators CLASS name LPAREN testlist RPAREN COLON suite { ClassDef ($3, to_list $5, $8, $1, annot $2) }

arglist:
  | argument { [$1], [], None, None }
  | argument COMMA { [$1], [], None, None }
  | argument COMMA arglist
      { match $3 with
        | args, keywords, starargs, kwargs ->
            $1::args, keywords, starargs, kwargs }
  | starargs { $1 }

argument:
  | test { $1 }
  | test gen_for { GeneratorExp ($1, $2, annot_of_expr $1) }

keyword:
  | test EQ test
      { match $1 with
        | Name (id, _, _) -> (id, $3)
        | _ -> raise Parsing.Parse_error }

starargs:
  | MULT test { [], [], Some $2, None }
  | MULT test COMMA keywords
      { match $4 with
        | args, keywords, _, kwargs ->
            args, keywords, Some $2, kwargs }
  | keywords { $1 }

keywords:
  | keyword { [], [$1], None, None }
  | keyword COMMA { [], [$1], None, None }
  | keyword COMMA keywords
      { match $3 with
        | args, keywords, starargs, kwargs ->
            args, $1::keywords, starargs, kwargs }
  | POW test { [], [], None, Some $2 }

list_for:
  | list_for1 { [$1] }
  | list_for1 list_for { $1::$2 }

list_for1:
  | FOR exprlist IN testlist_safe list_if_list { tuple_expr_store $2, tuple_expr $4, $5 }

list_if:
  | IF old_test { $2 }

list_if_list:
  | { [] }
  | list_if list_if_list { $1::$2 }

gen_for:
  | gen_for1 { [$1] }
  | gen_for1 gen_for { $1::$2 }

gen_for1:
  | FOR exprlist IN or_test gen_if_list { tuple_expr_store $2, $4, $5 }

gen_if:
  | IF old_test { $2 }

gen_if_list:
  | { [] }
  | gen_if gen_if_list { $1::$2 }

testlist1:
  | test { singleton $1 }
  | test COMMA testlist1 { cons $1 $3 }

yield_expr:
  | YIELD { Yield (None, annot $1) }
  | YIELD testlist_expr { Yield (Some $2, annot $1) }

name:
  | NAME { fst $1 }

%%
end
