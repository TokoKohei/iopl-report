open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "let rec f = fun x -> x and g = fun y -> y in f 3;;"; expected = IntV 3 };
  { input = "let rec f = fun x -> x and g = fun y -> y;;\nf 3;;"; expected = IntV 3 };
  { input = "let rec even = fun n -> if n < 1 then true else odd (n + -1) and\n         odd = fun n -> if n < 1 then false else even (n + -1) in\n         odd 3;;"; expected = BoolV true };
  { input = "let rec even = fun n -> if n < 1 then true else odd (n + -1) and\n         odd = fun n -> if n < 1 then false else even (n + -1) in\n         even 3;;"; expected = BoolV false };
  { input = "let rec even = fun n -> if n < 1 then true else odd (n + -1) and\n         odd = fun n -> if n < 1 then false else even (n + -1) in\n         odd 4;;"; expected = BoolV false };
  { input = "let rec even = fun n -> if n < 1 then true else odd (n + -1) and\n         odd = fun n -> if n < 1 then false else even (n + -1) in\n         even 4;;"; expected = BoolV true };
  { input = "let rec even = fun n -> if n < 1 then true else odd (n + -1) and\n         odd = fun n -> if n < 1 then false else even (n + -1);;\n         odd 3;;"; expected = BoolV true };
  { input = "let rec even = fun n -> if n < 1 then true else odd (n + -1) and\n         odd = fun n -> if n < 1 then false else even (n + -1);;\n         even 3;;"; expected = BoolV false };
  { input = "let rec even = fun n -> if n < 1 then true else odd (n + -1) and\n         odd = fun n -> if n < 1 then false else even (n + -1);;\n         odd 4;;"; expected = BoolV false };
  { input = "let rec even = fun n -> if n < 1 then true else odd (n + -1) and\n         odd = fun n -> if n < 1 then false else even (n + -1);;\n         even 4;;"; expected = BoolV true };
  { input = 
      "let rec f = fun x -> if x < 1 then true else if x < 2 then false else if x < 3 then false else h (x + -1)\n\
       and g = fun x -> if x < 1 then false else if x < 2 then true else if x < 3 then false else f (x + -1)\n\
       and h = fun x -> if x < 1 then false else if x < 2 then false else if x < 3 then true else g (x + -1);;\n\
       f 12;;"; 
    expected = BoolV true };
  { input = 
      "let rec f = fun x -> if x < 1 then true else if x < 2 then false else if x < 3 then false else h (x + -1)\n\
       and g = fun x -> if x < 1 then false else if x < 2 then true else if x < 3 then false else f (x + -1)\n\
       and h = fun x -> if x < 1 then false else if x < 2 then false else if x < 3 then true else g (x + -1);;\n\
       g 13;;"; 
    expected = BoolV true };
  { input = 
      "let rec f = fun x -> if x < 1 then true else if x < 2 then false else if x < 3 then false else h (x + -1)\n\
       and g = fun x -> if x < 1 then false else if x < 2 then true else if x < 3 then false else f (x + -1)\n\
       and h = fun x -> if x < 1 then false else if x < 2 then false else if x < 3 then true else g (x + -1);;\n\
       h 14;;"; 
    expected = BoolV true };
  { input = 
      "let rec f = fun x -> if x < 1 then true else if x < 2 then false else if x < 3 then false else h (x + -1)\n\
       and g = fun x -> if x < 1 then false else if x < 2 then true else if x < 3 then false else f (x + -1)\n\
       and h = fun x -> if x < 1 then false else if x < 2 then false else if x < 3 then true else g (x + -1) in\n\
       f 12;;"; 
    expected = BoolV true };
  { input = 
      "let rec f = fun x -> if x < 1 then true else if x < 2 then false else if x < 3 then false else h (x + -1)\n\
       and g = fun x -> if x < 1 then false else if x < 2 then true else if x < 3 then false else f (x + -1)\n\
       and h = fun x -> if x < 1 then false else if x < 2 then false else if x < 3 then true else g (x + -1) in\n\
       g 13;;"; 
    expected = BoolV true };
  { input = 
      "let rec f = fun x -> if x < 1 then true else if x < 2 then false else if x < 3 then false else h (x + -1)\n\
       and g = fun x -> if x < 1 then false else if x < 2 then true else if x < 3 then false else f (x + -1)\n\
       and h = fun x -> if x < 1 then false else if x < 2 then false else if x < 3 then true else g (x + -1) in\n\
       h 14;;"; 
    expected = BoolV true };
  { input = 
      "let rec f = fun x -> if x < 1 then true else if x < 2 then false else if x < 3 then false else h (x + -1)\n\
       and g = fun x -> if x < 1 then false else if x < 2 then true else if x < 3 then false else f (x + -1)\n\
       and h = fun x -> if x < 1 then false else if x < 2 then false else if x < 3 then true else g (x + -1);;\n\
       f 13;;"; 
    expected = BoolV false };
  { input = 
      "let rec f = fun x -> if x < 1 then true else if x < 2 then false else if x < 3 then false else h (x + -1)\n\
       and g = fun x -> if x < 1 then false else if x < 2 then true else if x < 3 then false else f (x + -1)\n\
       and h = fun x -> if x < 1 then false else if x < 2 then false else if x < 3 then true else g (x + -1);;\n\
       g 14;;"; 
    expected = BoolV false };
  { input = 
      "let rec f = fun x -> if x < 1 then true else if x < 2 then false else if x < 3 then false else h (x + -1)\n\
       and g = fun x -> if x < 1 then false else if x < 2 then true else if x < 3 then false else f (x + -1)\n\
       and h = fun x -> if x < 1 then false else if x < 2 then false else if x < 3 then true else g (x + -1);;\n\
       h 15;;"; 
    expected = BoolV false };
  { input = 
      "let rec f = fun x -> if x < 1 then true else if x < 2 then false else if x < 3 then false else h (x + -1)\n\
       and g = fun x -> if x < 1 then false else if x < 2 then true else if x < 3 then false else f (x + -1)\n\
       and h = fun x -> if x < 1 then false else if x < 2 then false else if x < 3 then true else g (x + -1) in\n\
       f 13;;"; 
    expected = BoolV false };
  { input = 
      "let rec f = fun x -> if x < 1 then true else if x < 2 then false else if x < 3 then false else h (x + -1)\n\
       and g = fun x -> if x < 1 then false else if x < 2 then true else if x < 3 then false else f (x + -1)\n\
       and h = fun x -> if x < 1 then false else if x < 2 then false else if x < 3 then true else g (x + -1) in\n\
       g 14;;"; 
    expected = BoolV false };
  { input = 
      "let rec f = fun x -> if x < 1 then true else if x < 2 then false else if x < 3 then false else h (x + -1)\n\
       and g = fun x -> if x < 1 then false else if x < 2 then true else if x < 3 then false else f (x + -1)\n\
       and h = fun x -> if x < 1 then false else if x < 2 then false else if x < 3 then true else g (x + -1) in\n\
       h 15;;"; 
    expected = BoolV false };
];;

let () = ignore(run_test_tt_main (
    "ex3.5.2" >:::
    gen_eval_tests dataset_for_eval
  ))
