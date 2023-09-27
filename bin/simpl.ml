open Interp.Main

let () =
  let str = read_line () in
  (* let str = "(fun x -> (fun y -> x + y) 3) 4" in *)
  let result = interp str in
  print_string result