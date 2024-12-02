open OUnit2
open Final_Project.Stock_logic
open Final_Project
open Lwt.Infix

(* Helper function for rounding to two decimal places *)
let round_to_hundredths f =
  Float.(round (f *. 100.) /. 100.)

(* Moving Average Test *)
let test_moving_average ticker s_date e_date expected _ =
  let json_data = Lwt_main.run (fetch_stock_data ticker) in (* Run the async fetch_stock_data synchronously *)
  let stock_data = extract_prices json_data s_date e_date in 
  let moving_average = calculate_moving_average stock_data 7 in
  assert_equal ~printer:string_of_float expected (round_to_hundredths moving_average)

(* Financial Ratios Test *)
let test_ratios _ =
  let symbol = "AAPL" in

  (* Fetch balance sheet and income statement *)
  let balance_promise = Ratios.fetch_stock_data symbol "BALANCE_SHEET" in
  let income_promise = Ratios.fetch_stock_data symbol "INCOME_STATEMENT" in

  (* Run test synchronously *)
  Lwt_main.run (
    balance_promise >>= fun balance_json ->
    income_promise >>= fun income_json ->

    let balance = Ratios.extract_data balance_json in
    let income = Ratios.extract_data income_json in

    (* Select the most recent year for testing *)
    let balance_current = Ratios.select_year balance 2023 in
    let income_current = Ratios.select_year income 2023 in

    (* Test Liquidity Ratios *)
    let liquidity_ratios = Ratios.liquidity balance_current in
    Printf.printf "Liquidity Ratios:\n";
    List.iter (fun (name, value) -> Printf.printf "  %s: %f\n" name value) liquidity_ratios;

    (* Test Efficiency Ratios *)
    let efficiency_ratios = Ratios.efficiency balance_current income_current in
    Printf.printf "\nEfficiency Ratios:\n";
    List.iter (fun (name, value) -> Printf.printf "  %s: %f\n" name value) efficiency_ratios;

    (* Test Leverage Ratios *)
    let leverage_ratios = Ratios.leverage balance_current income_current in
    Printf.printf "\nLeverage Ratios:\n";
    List.iter (fun (name, value) -> Printf.printf "  %s: %f\n" name value) leverage_ratios;

    (* Test Profitability Ratios *)
    let profitability_ratios = Ratios.profitability balance_current income_current in
    Printf.printf "\nProfitability Ratios:\n";
    List.iter (fun (name, value) -> Printf.printf "  %s: %f\n" name value) profitability_ratios;

    Lwt.return_unit
  )

(* Combine Tests into a Suite *)
let tests =
  "Combined Test Suite"
  >::: [
         "Test AAPL Moving Average" >:: test_moving_average "AAPL" "2024-10-18" "2024-10-29" 233.16;
         "Test Financial Ratios" >:: test_ratios;
       ]

(* Run all tests *)
let () = run_test_tt_main tests
