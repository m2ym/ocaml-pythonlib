let () =
  Pythonlib.Pretty.print_mod (Pythonlib.Parser2.parse_from_channel stdin)
