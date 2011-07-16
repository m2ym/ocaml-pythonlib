open Lang_python

let () =
  Dump.print_mod (Parse.parse_from_channel stdin)
