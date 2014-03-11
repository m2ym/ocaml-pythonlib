open Pythonlib.Simpl
open Pythonlib.Ast

let tmp_prefix = "__"

let is_tmpid =
  let len = String.length tmp_prefix in
  fun x ->
    let l = String.length x in
    let rec d i =
      if i>=l then true
      else
        if i>=len then
          (match x.[i] with
          | '0'..'9' -> d (i+1)
          | _ -> false)
        else
          (x.[i] = tmp_prefix.[i]) && d (i+1) in
    d 0

let list_count f =
  let rec c n = function
    | [] -> n
    | hd :: tl -> c (if f hd then n+1 else n) tl
  in
  c 0

let preferred a b =
  let f x =
    match x with
    | Name(x, Param, _) -> is_tmpid x
    | _ -> assert false in
  let fst (x, _, _, _) = x in
  let score_a = list_count f (fst a) in
  let score_b = list_count f (fst b) in
  if score_a > score_b then b else a

let same_args formal effective kw star kwargs =
  kw=[]
  && star=None
    && kwargs=None
      && match formal with
      | (formal, None, None, []) ->
        let is_in n =
          let rec loop l =
            match l with
            | [] -> false
            | Name(m, _, _) :: tl -> n=m || loop tl
            | _ :: tl -> loop tl in
          loop in
        let rec s f e =
          match f with
          | [] -> e=[]
          | Name(n, Param, _) :: tlf when not(is_in n tlf) ->
            (match e with
            | Name(n', Load, _) :: tle when n=n' -> s tlf tle
            | _ -> false)
          | _ -> false in
        s formal effective
      | _ -> false

let se = function
  | Lambda(args,
           Call(Lambda(args', body', al'), eargs, kw, star, kwargs, ac), al)
      when same_args args eargs kw star kwargs
        -> Some (Lambda(preferred args args', body', al'))
  | _ -> None

let ss = function
  | FunctionDef(id, args,
                [Return(Some(Call(Lambda(args', body', al), eargs, kw, star, kwargs, ac)), ar)], deco, af)
      when same_args args eargs kw star kwargs
        -> Some (FunctionDef(id, preferred args args',
                             [Return(Some(body'), ar)], deco, af))
  | _ -> None

let s = {
  simpl_expr = Some se;
  simpl_stmt = Some ss }

let () =
  Pythonlib.Pretty.print_mod (simpl s (Pythonlib.Parser2.parse_from_channel stdin))
