let () =
  try
  let filename = Sys.argv.(1) in
  let system = Parser.parse_file filename in
  print_endline (System.string_of_system system)

  with
  | Invalid_argument _ -> print_endline "usage: expert [filename]"
  | Sys_error msg -> print_endline msg