open OUnit2

let test1 ctx = assert_equal 1 1

let suite =
  "codegen tests">:::
  ["test1">:: test1;];;

let () = run_test_tt_main suite
