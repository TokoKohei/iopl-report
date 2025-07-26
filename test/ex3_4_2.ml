open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "(+) 1 2;;"; expected = IntV 3 };
  { input = "( +) 1 2;;"; expected = IntV 3 };
  { input = "(  +) 1 2;;"; expected = IntV 3 };
  { input = "(   +) 1 2;;"; expected = IntV 3 };
  { input = "(+ ) 1 2;;"; expected = IntV 3 };
  { input = "(+  ) 1 2;;"; expected = IntV 3 };
  { input = "(+   ) 1 2;;"; expected = IntV 3 };
  { input = "(     +     ) 1 2;;"; expected = IntV 3 };
  { input = "( *) 3 3;;"; expected = IntV 9 };
  { input = "(<) 10 -3;;"; expected = BoolV false };
  { input = "let threetimes = fun f -> fun x -> f (f x x) (f x x) in threetimes (+) 5;;"; expected = IntV 20};
  { input = "let threetimes = fun f -> fun x -> f (f x x) (f x x) in threetimes ( * ) 5;;"; expected = IntV 625};
  { input = "let comparable = fun f -> fun x -> fun y -> (f x y) || (f y x) in comparable (<) 1 2;;"; expected = BoolV true }
];;

let () = ignore(run_test_tt_main (
    "ex3.4.2" >:::
    gen_eval_tests dataset_for_eval
  ))