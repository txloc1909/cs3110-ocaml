open OUnit2
open Sum
open Weekday
open Patterns

let make_sum_test name expected_output input =
    name >:: (fun _ -> assert_equal expected_output (sum input) ~printer:string_of_int)

let make_weekday_test name expected_output input =
    name >:: (fun _ -> assert_equal expected_output (next_weekday input))

let sum_tests = "test suite for sum" >::: [
    make_sum_test "empty"        0 [];
    make_sum_test "singleton"    1 [1];
    make_sum_test "two_elements" 3 [1; 2];
]

let weekday_tests = "test suite for next_weekday" >::: [
    make_weekday_test "tue_after_mon" Tuesday Monday;
    make_weekday_test "wed_after_tue" Wednesday Tuesday;
    make_weekday_test "thu_after_wed" Thursday Wednesday;
    make_weekday_test "fri_after_thu" Friday Thursday;
    make_weekday_test "sat_after_fri" Saturday Friday;
    make_weekday_test "sun_after_sat" Sunday Saturday;
    make_weekday_test "mon_after_sun" Monday Sunday;
]

let patterns_tests = "test suite for patterns" >::: [
    "first_elem_bigred" >:: (fun _ -> assert (first_bigred ["bigred"; "foo";]));
    "empty_list_bigred" >:: (fun _ -> assert (not (first_bigred [])));
    "first_elem_not_bigred" >:: (fun _ -> assert (not (first_bigred ["foo"; "bar"; "baz"])));

    "len0" >:: (fun _ -> assert (not (len_2or4 [])));
    "len1" >:: (fun _ -> assert (not (len_2or4 ["foo"])));
    "len2" >:: (fun _ -> assert (len_2or4 [1.0; 2.0]));
    "len3" >:: (fun _ -> assert (not (len_2or4 [true; true; false])));
    "len4" >:: (fun _ -> assert (len_2or4 [1; 2; 3; 4]));
    "len5" >:: (fun _ -> assert (not (len_2or4 [1; 2; 3; 4; 5])));

    "firsttwo_empty" >:: (fun _ -> assert (not (first_two_equal [])));
    "firsttwo_one" >:: (fun _ -> assert (not (first_two_equal ["foo"])));
    "firsttwo_equal" >:: (fun _ -> assert (first_two_equal [1.0; 1.0]));
    "firsttwo_not_eq" >:: (fun _ -> assert (not (first_two_equal [1; 2; 3])));
]

let _ = run_test_tt_main sum_tests
let _ = run_test_tt_main weekday_tests
let _ = run_test_tt_main patterns_tests
