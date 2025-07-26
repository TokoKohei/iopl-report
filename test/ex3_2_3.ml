open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "true && false;;"; expected = BoolV false };
  { input = "true || false;;"; expected = BoolV true };
  { input = "true || false && false;;"; expected = BoolV true };
  { input = "false && false || true;;"; expected = BoolV true };
  { input = "true && false || true;;"; expected = BoolV true };
  { input = "true && true && true;;"; expected = BoolV true };
  { input = "false || false || false;;"; expected = BoolV false };
  { input = "1 < 2 && false;;"; expected = BoolV false };
  { input = "if 1 < 2 && 3 < 4 then true || false else true && false;;"; expected = BoolV true };
  { input = "if 1 < 2 && 3 < 1 then true || false else true && false;;"; expected = BoolV false};
  { input = "false && undef;;"; expected = BoolV false };
  { input = "true || undef;;"; expected = BoolV true };
  { input = "2 < 1 && undef;;"; expected = BoolV false };
  { input = "1 < 2 || undef;;"; expected = BoolV true };
];;

let dataset_for_evalerror = [
  { input = "undef;;" };
  { input = "true && undef;;" };
  { input = "false || undef;;" };
];;

let () = ignore(run_test_tt_main (
    "ex3.2.3" >:::
    gen_eval_tests dataset_for_eval
    @ gen_evalerror_tests dataset_for_evalerror
  ))
