module List = struct
  include List

  let mapi f list =
    let rec loop n = function
      | [] -> []
      | x :: xs -> f n x :: loop (n + 1) xs
    in loop 0 list

  let partition_at i list =
    let rec loop i list =
      match i, list with
      | 0, _ -> [], list
      | _, [] -> [], []
      | _, x :: xs ->
          let l, r = loop (pred i) xs in
            (x :: l), r
    in loop i list
end
