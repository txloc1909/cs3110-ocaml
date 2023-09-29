open OUnit2
open Sum
open Weekday
open Patterns
open Library

let make_sum_test name expected_output input =
    name >:: (fun _ -> assert_equal expected_output (sum input) ~printer:string_of_int)

let make_simple_test name func input expected_output =
    name >:: (fun _ -> assert_equal expected_output (func input))

let sum_tests = "test suite for sum" >::: [
    make_sum_test "empty"        0 [];
    make_sum_test "singleton"    1 [1];
    make_sum_test "two_elements" 3 [1; 2];
]

let weekday_tests = "test suite for next_weekday" >::: [
    make_simple_test "tue_after_mon" next_weekday Monday Tuesday;
    make_simple_test "wed_after_tue" next_weekday Tuesday Wednesday;
    make_simple_test "thu_after_wed" next_weekday Wednesday Thursday;
    make_simple_test "fri_after_thu" next_weekday Thursday Friday;
    make_simple_test "sat_after_fri" next_weekday Friday Saturday;
    make_simple_test "sun_after_sat" next_weekday Saturday Sunday;
    make_simple_test "mon_after_sun" next_weekday Sunday Monday;
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

let library_tests = "test suite for library" >::: [
    make_simple_test "5th_elem_0" fifth_elem [] 0;
    make_simple_test "5th_elem_1" fifth_elem [1] 0;
    make_simple_test "5th_elem_2" fifth_elem [1; 2] 0;
    make_simple_test "5th_elem_5" fifth_elem [1; 2; 3; 4; 5] 5;
    make_simple_test "5th_elem_more" fifth_elem [1; 2; 3; 4; 5; 6] 5;
]

let _ = run_test_tt_main sum_tests
let _ = run_test_tt_main weekday_tests
let _ = run_test_tt_main patterns_tests
let _ = run_test_tt_main library_tests
