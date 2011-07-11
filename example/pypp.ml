open Lang_python

let () =
  Pretty.print_mod (Parse.parse_from_channel stdin)
