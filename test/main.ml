open OUnit2
open Jocalf

(* You're free to change any of the code in this file, but the test
   cases themselves demonstrate the exact output your interpreter has to
   produce. So I don't recommend changing the strings in the provided
   test cases. *)

(* All of these provided tests will currently fail because you have not
   yet implemented interpretation of any of these syntactic forms. A
   completed interpreter should pass all of them, though. OCaml allows
   {|...|} as a syntax for strings in which the ... can contain
   unescaped quotes. This is super useful for constructing test cases,
   as shown below. *)
let provided_tests =
  [
    ("int constant", {|42|}, "42");
    ("negative int\n constant", {|-1|}, "-1");
    (let str_max_int = string_of_int max_int in
     ("max int", str_max_int, str_max_int));
    (let str_min_int = string_of_int min_int in
     ("min int", str_min_int, str_min_int));
    ("true", {|true|}, "true");
    ("false", {|false|}, "false");
    ("undefined", {|undefined|}, "undefined");
    ("magic word", {|"xyzzy"|}, {|"xyzzy"|});
    ("div by 0", {|4/0|}, {|Exception: "Error: Division by zero"|});
    ("mod by 0", {|4 mod 0|}, {|Exception: "Error: Division by zero"|});
    ( "unbound var",
      {|let x = 0 in y|},
      {|Exception: "Error: Unbound variable"|} );
    (* ("throw", {|throw 0|}, "Exception: 0"); *)
    ("anonymous function", {|fun (x) -> 0|}, "<function>");
    ( "apply non-function",
      {|0 0|},
      {|Exception: "Error: Not a function"|} );
    ( "apply wrong arity",
      {|(fun (x) -> 0) 1 2|},
      {|Exception: "Error: Wrong number of arguments"|} );
    (*("ref", {|ref 0|}, "<reference>"); ( "assign non\n location", {|1
      := 0|}, {|Exception: "Error: Assignment to non-location"|} );
      ("object", {|{"x":1}|}, "<object>"); ("length", {|length
      "bigred"|}, "6"); ("typeof int", {|typeof 42|}, {|"int"|});
      ("typeof string", {|typeof "xyzzy"|}, {|"string"|}); ("typeof
      bool", {|typeof true|}, {|"bool"|}); ("typeof undefined", {|typeof
      undefined|}, {|"undefined"|}); ("typeof object", {|typeof
      {"x":1}|}, {|"object"|}); ("typeof\n reference", {|typeof (ref
      0)|}, {|"reference"|}); ("typeof\n closure", {|typeof (fun (x) ->
      0)|}, {|"function"|}); ("typeof\n extern", {|typeof length|},
      {|"function"|}); ("defined", {|is_defined undefined|}, "false");
      ("has field", {|has_field {"x":1} "x"|}, "true"); *)
  ]

(* Here's a start at some test cases of your own. *)

let my_tests =
  [
    ("true constant", {|true|}, "true");
    ("false constant", {|false|}, "false");
    ("integer positive", {|1|}, "1");
    ("undefined", {|undefined|}, "undefined");
    ("integer negative", {|-10|}, "-10");
    ("true int", {|not 10|}, "false");
    ("true str", {|not "100"|}, "false");
    ("not true", {|not true|}, "false");
    ("false 0", {|not 0|}, "true");
    ("not false", {|not false|}, "true");
    ("false empty string", {|not ""|}, "true");
    ("Simple add", {| 10 + 10|}, "20");
    ("complex add", {| 10 + 10 + 20 + 30 + 40|}, "110");
    ("add a negative", {| 10 + -100|}, "-90");
    ("Simple string add", {| "10" + "10"|}, {|"1010"|});
    (" string int add", {| 20 + "10" + 20|}, {|"201020"|});
    ("Simple mult", {| 10 * 10|}, "100");
    ("complex mult", {| 10 * 10* 10 * 10|}, "10000");
    ("mult a negative", {| 10 * -10|}, "-100");
    ("mult a negative2", {| -10 * 10|}, "-100");
    ("mult two negative", {| -2 * -10|}, "20");
    ("mult undefined", {| -2 * "zzz"|}, "undefined");
    ("Simple div", {| 20 / 10|}, "2");
    ("Simple div", {| 20 / "zzz"|}, "undefined");
    ("complex div", {| 1000 / 10 / 10 / 10|}, "1");
    ("div a negative", {| 10 / -10|}, "-1");
    ("div a negative2", {| -10 / 10|}, "-1");
    ("div two negative", {| -2 / -1|}, "2");
    ("Simple mod", {| 130 mod 20|}, "10");
    ("Simple mod", {| 20 mod "zzz"|}, "undefined");
    ("complex mod", {| 125 mod 20 mod 4|}, "1");
    ("mod a negative", {| -10 mod 4|}, "-2");
    ("mod a negative2", {| 20 mod -19|}, "1");
    ("mod two negative", {| -5 mod -4|}, "-1");
    ("Simple lt", {| 1 < 2 |}, "true");
    ("lt false", {| 10 < 2 |}, "false");
    ("lt false2", {| 2 < 2 |}, "false");
    ("lt negative", {| -10 < 100 |}, "true");
    ("lt negative2", {| -10 < -100 |}, "false");
    ("Simple Gt", {| 2 > 1 |}, "true");
    ("Gt false", {| 2 > 2 |}, "false");
    ("Gt false2", {| 2 > 100 |}, "false");
    ("Gt negative", {| 10 > -10 |}, "true");
    ("Gt negative2", {| -10 > -100 |}, "true");
    ("Simple lte", {| 1 <= 2 |}, "true");
    ("lte false", {| 10 <= 2 |}, "false");
    ("lte equal", {| 2 <= 2 |}, "true");
    ("lte negative", {| -10 <= 100 |}, "true");
    ("lte negative2", {| -10 <= -100 |}, "false");
    ("Simple lte", {| 20 <= "zzz"|}, "false");
    ("Simple Gte", {| 2 >= 1 |}, "true");
    ("Gte false", {| 2 >= 2 |}, "true");
    ("Gte false2", {| 2 >= 100 |}, "false");
    ("Gte negative", {| 10 >= -10 |}, "true");
    ("Gte negative2", {| -10 >= -100 |}, "true");
    ("Simple eq", {| 2 = 2 |}, "true");
    ("eq false", {| 2 = 1 |}, "false");
    ("eq negative", {| 100 = -100 |}, "false");
    ("eq negative", {| -10 = 10 |}, "false");
    ("eq negative2", {| -10 = -100 |}, "false");
    ("eq string", {| "a" = "a" |}, "true");
    ("eq string2", {| "a" = "b" |}, "false");
    ("eq bool", {| true = true |}, "true");
    ("eq bool2", {| true = false |}, "false");
    ("eq string", {| "a" = "a" |}, "true");
    ("eq string2", {| "a" = "b" |}, "false");
    ("eq undefined", {| undefined = undefined |}, "true");
    ("eq undefined", {| undefined = "b" |}, "false");
    ("eq string int", {| "1" = 1 |}, "true");
    ("eq string int2", {| 2 = "10" |}, "false");
    ("eq bool int", {| 0 = false |}, "true");
    ("eq bool int2", {| 1 = false |}, "false");
    ("eq bool string", {| "0" = false |}, "true");
    ("eq bool string2", {| "1" = false |}, "false");
    ("eq coml", {| (10*10) = (1000/10) |}, "true");
    ("eq coml", {| (10*10) = "zzz" |}, "false");
    (*add undefined test*)
    ("Simple eq", {| 2 != 2 |}, "false");
    ("neq false", {| 2 != 1 |}, "true");
    ("neq negative", {| 100 != -100 |}, "true");
    ("neq negative", {| -10 != 10 |}, "true");
    ("neq negative2", {| -10 != -100 |}, "true");
    ("neq string", {| "a" != "a" |}, "false");
    ("neq string2", {| "a" != "b" |}, "true");
    ("neq bool", {| true != true |}, "false");
    ("neq bool2", {| true != false |}, "true");
    ("neq string", {| "a" != "a" |}, "false");
    ("neq string2", {| "a" != "b" |}, "true");
    ("neq undefined", {| undefined != undefined |}, "false");
    ("neq undefined", {| undefined != "b" |}, "true");
    ("neq string int", {| "1" != 1 |}, "false");
    ("neq string int2", {| 2 != "10" |}, "true");
    ("neq bool int", {| 0 != false |}, "false");
    ("neq bool int2", {| 1 != false |}, "true");
    ("neq bool string", {| "0" != false |}, "false");
    ("neq bool string2", {| "1" != false |}, "true");
    ("neq compl", {| (10*10) != (1000/10) |}, "false");
    ("eq coml", {| (10*10) != "zzz" |}, "true");
    ("eq exact int", {| 10 == 10 |}, "true");
    ("eq exact int2", {| 10 == 100 |}, "false");
    ("eq exact str", {| "a" == "b" |}, "false");
    ("eq exact str2", {| "a" == "a" |}, "false");
    ("eq exact bool", {| true == true |}, "true");
    ("eq exact bool2", {| false == true |}, "false");
    ("neq exact int", {| 10 !== 10 |}, "false");
    ("neq exact int2", {| 10 !== 100 |}, "true");
    ("neq exact str", {| "a" !== "b" |}, "true");
    ("neq exact str2", {| "a" !== "a" |}, "true");
    ("neq exact bool", {| true !== true |}, "false");
    ("neq exact bool2", {| false !== true |}, "true");
    ("Simple and", {| true && true |}, "true");
    ("and false", {| false && true |}, "false");
    ("and true compl", {| true && true && true|}, "true");
    ("and true compl", {| true && false && true|}, "false");
    ("and true int", {| 1 && true|}, "true");
    ("and true int", {| 0 && true|}, "false");
    ("and true str", {| "1" && true|}, "true");
    ("and true str", {| true && ""|}, "false");
    ("and true bop", {| (1 + 2) && (1 -1)|}, "false");
    ("Simple or", {| true || false |}, "true");
    ("or false", {| false || false |}, "false");
    ("or true compl", {| true || false || false|}, "true");
    ("or false compl", {| false || false || false|}, "false");
    ("or true int", {| 1 || false|}, "true");
    ("or true int", {| 0 || 0|}, "false");
    ("or true str", {| "1" || false|}, "true");
    ("or true str", {| true || ""|}, "true");
    ("or true bop", {| (1 + 2) || (1 -1)|}, "true");
    ("simple if statement", {| if true then 1 else 2 |}, "1");
    ("simple if statement false", {| if false then 1 else 2 |}, "2");
    ("simple if statement str", {| if "1" then 1 else 2 |}, "1");
    ("simple if statement str false", {| if "" then 1 else 2 |}, "2");
    ("simple if statement int", {| if 100 then 1 else 2 |}, "1");
    ("simple if statement int false", {| if 0 then 1 else 2 |}, "2");
    ("if statement undefined", {| if 0 then 1 |}, "undefined");
    ("if statement undefined", {| if 1 then 1 |}, "1");
    ("if statement eval", {| if 0 = 0 then 1 |}, "1");
    ("if statement eval", {| if true && true && true then 1 |}, "1");
    ("simple let int", {| let x = 1 in x |}, "1");
    ("simple let str", {| let x = "hello" in x |}, {|"hello"|});
    ("simple let int", {| let x = true in x |}, "true");
    ("let int expr", {| let x = (1 + 2) in x + 4 |}, "7");
    ( "let int expr comp",
      {| let x = (1 + 2) in let y = 1 in x + y |},
      "4" );
    ( "let int expr comp",
      {| let x = (1 + 2) in (let x = 1 in x + x) + x |},
      "5" );
    ( "let int not in",
      {| let x = (1 + 2) in (let x = 1 in x + y) + x |},
      {|Exception: "Error: Unbound variable"|} );
    ("function", {| fun (x) -> x |}, "<function>");
    ("function app", {| (fun (x) -> x) 1 |}, "1");
    ("function app", {| (fun (x) -> (1 + 1)) 3 |}, "2");
    ("function app", {| (fun (x) -> (x)) (1+1) |}, "2");
    ("function app", {| (fun (x) -> (x + 1)) (1+1) |}, "3");
    ( "function app",
      {| (fun (x) -> if (x > 10) then (20) else (10)) (100 + 6) |},
      "20" );
    ("function app", {| (fun (x y) -> (x + 1)) (1+1) 1|}, "3");
    ("function app", {| (fun (x y) -> (x + y)) (1+1) 1|}, "3");
    ("typeof bool", {| typeof true |}, {|"bool"|});
    ("typeof int", {| typeof (1 + 1) |}, {|"int"|});
    ("typeof string", {| typeof "true" |}, {|"string"|});
  ]

let tests = provided_tests @ my_tests

let make_interp_expr_test n in_str out_str =
  n >:: fun _ ->
  assert_equal out_str (Interp.interp_expr in_str) ~printer:Fun.id

let suite =
  "suite"
  >::: List.map (fun (n, i, o) -> make_interp_expr_test n i o) tests

let _ = run_test_tt_main suite
