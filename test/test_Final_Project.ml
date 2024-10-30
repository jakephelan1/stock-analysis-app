open OUnit2
open Final_Project.Stock_logic

let round_to_hundredths f =
  Float.(round (f *. 100.) /. 100.)

let test_moving_average ticker s_date e_date expected _ =
  let json_data = Lwt_main.run (fetch_stock_data ticker) in (* Run the async fetch_stock_data synchronously *)
  let stock_data = extract_prices json_data s_date e_date in 
  let moving_average = calculate_moving_average stock_data 7 in
  assert_equal ~printer:string_of_float expected (round_to_hundredths moving_average)

let tests =
  "Stock Data Test Suite"
  >::: [
         "Test AAPL Moving Average" >:: test_moving_average "AAPL" "2024-10-18" "2024-10-29" 233.16;
       ]

let () = run_test_tt_main tests