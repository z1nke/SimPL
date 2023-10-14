open OUnit2
open Interp.Main

let make_i n i s =
  let x = string_of_int i in
  n >:: (fun _ -> assert_equal x (interp s);
                  assert_equal x (small_interp s))

let make_b n b s =
  let x = string_of_bool b in
  n >:: (fun _ -> assert_equal x (interp s);
                  assert_equal x (small_interp s))

let make_s n x s =
  n >:: (fun _ -> assert_equal x (interp s);
                  assert_equal x (small_interp s))

let make_err n err s =
  let f1 = fun () -> (interp s) in
  let f2 = fun () -> (small_interp s) in
  n >:: (fun _ -> assert_raises (Failure err) f1;
                  assert_raises (Failure err) f2)

let tests = [
  make_i "int" 22 "22";
  make_i "add" 22 "11+11";
  make_i "mul" 22 "2*11";
  make_i "mul of mul" 40 "2*2*10";
  make_i "mul on right" 22 "2+2*10";
  make_i "mul on left" 14 "2*2+10";
  make_i "nested add" 22 "(10 + 1) + (5 + 6)";
  make_i "sub" 20 "22-2";
  make_i "neg" 20 "-2+22";
  make_i "neg" (-20) "-2*10";
  make_i "neg2" (-20) "10*-2";
  make_i "neg3" (-40) "10*-2*2";
  make_i "sub" (-40) "-2*10-10*2";
  make_i "sub2" (-40) "-2+10*-2-18";
  make_i "div" 20 "40/2";
  make_i "div2" 20 "20*2/2";
  make_i "div3" 22 "2+40/2";
  make_i "mod" 0 "4%2";
  make_i "mod" 1 "10%3";
  make_i "mod" 1 "10%-3";

  make_b "true" true "true";
  make_b "false" true "true";
  make_b "lt" true "1<2";
  make_b "lt2" true "-2<1";
  make_b "lt2" false "2<-1";
  make_b "eq" true "1==1";
  make_b "eq2" false "1==2";
  make_b "eq3" true "-1==-1";
  make_b "eq4" false "-1==-2";
  make_err "bop_err" bop_err "true==1";

  make_i "if" 0 "if true then 0 else 1";
  make_i "if1" 1 "if false then 0 else 1";
  make_i "if2" 42 "if 42==42 then 42 else 0";
  make_b "if3" true "if true then true else false";
  make_err "if4" if_guard_err "if 1 then 42 else 0";

  make_i "let" 42 "let x = 0 in 42";
  make_i "let2" 43 "let x = 42 in x + 1";
  make_i "let3" 43 "let x = 42 in if x == 42 then x + 1 else x";
  make_i "let4" 22 "let x = 22 in if x == 42 then x + 1 else x";
  make_i "let5" 1 "let x = 0 in (let x = 1 in x)";
  make_i "let6" 0 "let x = 0 in (let y = 1 in x)";
  make_i "let7" 1 "let x = 0 in (let y = 1 in x + y)";
  make_b "let8" true "let x = 0 in true";
  make_b "let9" false "let x = 0 in false";
  make_i "let10" (-42) "let x = 42 in -x";
  make_i "let11" 8 "let x = 2 + 2 in x + x";
  make_i "let12" 11 "let x = 5 in ((let x = 6 in x) + x)";
  make_i "let13" 4 "let x = 1 in (let x = x + x in x + x)";
  make_i "let14" 3 "let x = 1 in (let y = x + 1 in x + y)";
  make_err "let15" unbound_var_err "x";
  make_err "let16" unbound_var_err "let x = 1 in y";

  make_i "let_if" 1
      "let x = 2 in (let y = 2 in (let z = x == y in if z then 1 else 0))";

  make_i "lambda" 43 "(fun x -> x + 1) 42";
  make_i "lambda2" 2 "let x = 1 in (fun x -> x + 1) x";
  make_i "lambda3" 1 "(fun x -> x * x) 1";
  make_i "lambda4" 4 "(fun x -> x * x) 2";
  make_i "lambda5" 7 "(fun x -> (fun y -> (x + y)) 3) 4";
  make_i "lambda6" 3 "let x = (fun x -> x + 1) in x 2";
  (* make_i "lambdaX1" 7 "fun x -> (fun y -> (x + y)) 3 4"; *)
  (* make_i "lambdaX2" 7 "(fun x -> (fun y -> (x + y))) 3 4"; *)
  
  make_s "lambda_val" str_lambda_val "fun x -> x + 1";

  make_err "lambda_err" apply_err "42 0";

  make_s "pair" "(0, 42)" "(0,42)";
  make_s "pair1" "(2, 42)" "(1+1, 42)";
  make_s "pair2" "(true, 42)" "(true,42)";
  make_s "pair3" (Format.sprintf "(%s, 42)" str_lambda_val)
         "(fun x -> x + 1, 42)";
  make_s "pair4" "(0, 42)" "let x = (0, 42) in x";
  make_s "pair5" "(0, (1, 2))" "(0, (1, 2))";
  make_s "pair6" "(0, (1, 2))" "let x = (1, 2) in (0, x)";

  make_i "car1" 0 "car (0, 42)";
  make_i "car2" 2 "car (1+1, 42)";
  make_b "car3" true "car (true, 42)";
  make_b "car4" true "car (1==1, 42)";
  make_b "car5" true "car (1<2, 42)";
  make_b "car6" false "car (1==2, 42)";
  make_i "car7" 42 "(car (fun x -> x + 1, 42)) 41";

  make_i "cdr1" 42 "cdr (0, 42)";
  make_i "cdr2" 2 "cdr (42, 1+1)";
  make_s "cdr3" "(2, 3)" "cdr (1,(2,3))";
  make_i "cdr4" 3 "cdr cdr (1, (2, 3))";
  make_i "cdr5" 2 "car cdr (1, (2, 3))";

  make_s "left1" "<left 42>" "Left 42";
  make_s "left2" "<left 2>" "Left (1+1)";
  make_s "left3" "<left 2>" "Left 1+1";
  make_s "right1" "<right 42>" "Right 42";
  make_s "right2" "<right 2>" "Right(1+1)";
  make_s "right3" "<right 2>" "Right1+1";

  make_i "match1" 42 "match Left 42 with Left x -> x | Right x -> x + 1";
  make_i "match2" 43 "match Right 42 with Left x -> x | Right x -> x + 1";
  make_i "match3" 42 "match Left ((fun x -> x + 1) 41)
                      with Left x -> x | Right x -> x + 1";
  make_i "match3" 42 "let x = Left ((fun x -> x + 1) 41) in
                      match x with Left x -> x | Right x -> x + 1";
  make_err "match_err" not_left_or_right_err
    "let x = 42 in match x with Left x -> x | Right x -> x + 1";
]

let _ = run_test_tt_main ("suite" >::: tests)