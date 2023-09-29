open OUnit2
open Sum
open Weekday

let make_sum_test name expected_output input =
    name >:: (fun _ -> assert_equal expected_output (sum input) ~printer:string_of_int)

let sum_tests = "test suite for sum" >::: [
    make_sum_test "empty"        0 [];
    make_sum_test "singleton"    1 [1];
    make_sum_test "two_elements" 3 [1; 2];
]

let weekday_tests = "test suite for next_weekday" >::: [
    "tue_after_mon" >:: (fun _ -> assert_equal Tuesday (next_weekday Monday));
    "wed_after_tue" >:: (fun _ -> assert_equal Wednesday (next_weekday Tuesday));
    "thu_after_wed" >:: (fun _ -> assert_equal Thursday (next_weekday Wednesday));
    "fri_after_thu" >:: (fun _ -> assert_equal Friday (next_weekday Thursday));
    "sat_after_fri" >:: (fun _ -> assert_equal Saturday (next_weekday Friday));
    "sun_after_sat" >:: (fun _ -> assert_equal Sunday (next_weekday Saturday));
    "mon_after_sun" >:: (fun _ -> assert_equal Monday (next_weekday Sunday));
]

let _ = run_test_tt_main sum_tests
let _ = run_test_tt_main weekday_tests
