open OUnit2
open Libcalc.Calc

let make_i n i s =
  n >:: (fun _ -> assert_equal (string_of_int i) (interp s))

let tests = [
  make_i "int" 22 "22";
  make_i "add" 22 "11+11";
  make_i "mul" 22 "2*11";
  make_i "mul of mul" 40 "2*2*10";
  make_i "mul on right" 22 "2+2*10";
  make_i "mul on left" 14 "2*2+10";
  make_i "nested add" 22 "(10 + 1) + (5 + 6)";
]

let _ = run_test_tt_main ("suite" >::: tests)