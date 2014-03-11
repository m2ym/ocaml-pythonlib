open Ast

exception Empty_statement_list

type 'a simplifier = {
  simpl_expr: ('a expr -> 'a expr option) option;
  simpl_stmt: ('a stmt -> 'a stmt option) option }

let simpl s =
  let sexpr =
    match s.simpl_expr with
    | None -> (fun _ -> None)
    | Some s -> s in
  let sstmt =
    match s.simpl_stmt with
    | None -> (fun _ -> None)
    | Some s -> s in
  let rec _expr e =
    let e' =
      (match e with
      | BoolOp(op, vals, ab) -> BoolOp(op, List.map _expr vals, ab)
      | BinOp(left, op, right, ab) -> BinOp(_expr left, op, _expr right, ab)
      | UnaryOp(op, operand, au) -> UnaryOp(op, _expr operand, au)
      | Lambda(args, body, al) -> Lambda(args, body, al)
      | IfExp(test, body, orelse, ai) -> IfExp(_expr test, _expr body, _expr orelse, ai)
      | Dict(keys, vals, ad) -> Dict(List.map _expr keys, List.map _expr vals, ad)
      | ListComp(elt, generators, al) -> ListComp(_expr elt, List.map _compr generators, al)
      | GeneratorExp(elt, generators, ag) -> GeneratorExp(_expr elt, List.map _compr generators, ag)
      | Yield(vals, ay) -> Yield(_optexpr vals, ay)
      | Compare(left, ops, right, ac) -> Compare(_expr left, ops, List.map _expr right, ac)
      | Call(func, args, keywords, star, kwarg, ac) -> Call(_expr func, List.map _expr args, List.map _keyword keywords, _optexpr star, _optexpr kwarg, ac)
      | Repr(vals, ar) -> Repr(_expr vals, ar)
      | Num _ | Str _ | Name _ -> e
      | Attribute(vals, attr, ctx, aa) -> Attribute(_expr vals, attr, ctx, aa)
      | Subscript(vals, slice, ctx, a) -> Subscript(_expr vals, _slice slice, ctx, a)
      | List(elts, ctx, al) -> List(List.map _expr elts, ctx, al)
      | Tuple(elts, ctx, at) -> Tuple(List.map _expr elts, ctx, at)) in
    match sexpr e' with
    | None -> e'
    | Some e'' -> _expr e''
  and _optexpr e =
    match e with
    | None -> e
    | Some e -> Some(_expr e)
  and _keyword (arg, vals) = (arg, _expr vals)
  and _slice s =
    match s with
    | Ellipsis -> s
    | Slice(lower, upper, step) -> Slice(_optexpr lower, _optexpr upper, _optexpr step)
    | ExtSlice(dims) -> ExtSlice(List.map _slice dims)
    | Index(vals) -> Index(_expr vals)
  and _compr (target, iter, ifs) = (_expr target, _expr iter, List.map _expr ifs) in
  let rec _stmt s =
    let s' =
      (match s with
      | FunctionDef(id, args, body, deco, af) -> FunctionDef(id, args, _stmt_list body, List.map _expr deco, af)
      | ClassDef(id, bases, body, deco, ac) -> ClassDef(id, List.map _expr bases, _stmt_list body, List.map _expr deco, ac)
      | Return(r, ar) -> Return(_optexpr r, ar)
      | Delete(e, ad) -> Delete(List.map _expr e, ad)
      | Assign(targets, vals, aa) -> Assign(List.map _expr targets, _expr vals, aa)
      | AugAssign(target, op, vals, aa) -> AugAssign(_expr target, op, _expr vals, aa)
      | Print(dest, vals, nl, ap) -> Print(_optexpr dest, List.map _expr vals, nl, ap)
      | For(target, iter, body, orelse, af) -> For(_expr target, _expr iter, _stmt_list body, List.map _stmt orelse, af)
      | While(test, body, orelse, aw) -> While(_expr test, _stmt_list body, List.map _stmt orelse, aw)
      | If(test, body, orelse, ai) -> If(_expr test, _stmt_list body, _stmt_list orelse, ai)
      | With(ctx, vars, body, aw) -> With(_expr ctx, _optexpr vars, _stmt_list body, aw)
      | Raise(typ, inst, tback, ar) -> Raise(_optexpr typ, _optexpr inst, _optexpr tback, ar)
      | TryExcept(body, handlers, orelse, at) -> TryExcept(_stmt_list body, List.map _handler handlers, List.map _stmt orelse, at)
      | TryFinally(body, final, at) -> TryFinally(_stmt_list body, List.map _stmt final, at)
      | Assert(test, msg, aa) -> Assert(_expr test, _optexpr msg, aa)
      | Import _ | ImportFrom _ | Global _ | Pass _ | Break _ | Continue _ -> s
      | Exec(body, globals, locals, ae) -> Exec(_expr body, _optexpr globals, _optexpr locals, ae)
      | Expr(v, ae) -> Expr(_expr v, ae)) in
    match sstmt s' with
    | None -> s'
    | Some s'' -> _stmt s''
  and _handler h =
    match h with
    | ExceptHandler(typ, name, body, ae) -> ExceptHandler(_optexpr typ, _optexpr name, List.map _stmt body, ae)
  and _stmt_list l =
    let rec loop l =
      match l with
      | [] -> []
      | ((Return _ | Raise _ | Pass _ | Break _ | Continue _) as s) :: _ -> [_stmt s]
      | s :: tl -> _stmt s :: _stmt_list l
    in
    match l with
    | [] -> raise Empty_statement_list
    | _ -> loop l in

  fun m ->
    match m with
    | Module(l, am) -> Module(List.map _stmt l, am)
    | Interactive(l, ai) -> Interactive(List.map _stmt l, ai)
    | Expression(e, ae) -> Expression(_expr e, ae)
    | Suite(l, a) -> Suite(List.map _stmt l, a)
