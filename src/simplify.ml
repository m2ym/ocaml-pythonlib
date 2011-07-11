open Ast
open Utils

let tmp_var_count = ref 0

let tmp_var () =
  incr tmp_var_count;
  Printf.sprintf "_tmp%d" !tmp_var_count

let rec simplify_mod = function
  | Module body ->
      Module (simplify_body body)
  | Interactive body ->
      Interactive (simplify_body body)
  | Expression expr ->
      Expression (simplify_expr expr)
  | Suite body ->
      Suite (simplify_body body)

and simplify_body = function
  | [] -> []

  | Assign (targets, value, pos) :: rest
      when (List.length targets) > 1 ->
      let var = tmp_var () in
      let var_store = Name (var, Store, dummy_pos) in
      let var_load = Name (var, Load, dummy_pos) in
      let var_asgn = Assign ([var_store], value, pos) in
      let asgns =
        List.map
          (fun target ->
             Assign ([target], var_load, dummy_pos))
          targets
      in simplify_body (var_asgn :: asgns) @ simplify_body rest

  | Assign ([List (elts, _, _)], value, pos) :: rest
  | Assign ([Tuple (elts, _, _)], value, pos) :: rest ->
      let var = tmp_var () in
      let var_store = Name (var, Store, dummy_pos) in
      let var_load = Name (var, Load, dummy_pos) in
      let var_asgn = Assign ([var_store], value, pos) in
      let asgns =
        List.mapi
          (fun i elt ->
             let slice = Index (Num (Int i, dummy_pos)) in
             let subs = Subscript (var_load, slice, Load, dummy_pos) in
               Assign ([elt], subs, pos))
          elts
      in simplify_body (var_asgn :: asgns) @ simplify_body rest

  | s :: ss ->
      s :: simplify_body ss

and simplify_expr expr = expr
