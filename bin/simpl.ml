open SimPL.Main

let () =
  let str = read_line () in
  let result = interp str in
  print_string result