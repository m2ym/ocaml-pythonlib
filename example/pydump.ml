open Lang_python

let () =
  Dump.dump_mod (Parse.parse_from_channel stdin)
