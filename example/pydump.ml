let () =
  let modl = Lang_python.Parse.parse_from_channel stdin in
    Lang_python.Dump.dump_mod modl
