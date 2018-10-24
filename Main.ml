let () =
  try
  let filename = Sys.argv.(1) in
  let system = Parser.parse_file filename in
  (* let system = System.dummy_system in *)
  print_endline (System.string_of_system system)
  with
  | Failure err -> raise (Failure err)
  | Parser.Parsing_exception str -> Printf.printf "Invalid instruction near: {%s}\n" str
  | Invalid_argument _ -> print_endline "usage: expert [filename]"
  | Sys_error msg -> print_endline msg