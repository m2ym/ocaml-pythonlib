module List = struct
  include List

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
