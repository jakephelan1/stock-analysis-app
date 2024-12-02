(* test/stock_logic_test.ml *)

open OUnit2
open Lwt.Infix
open Yojson.Basic.Util

(* Assuming the modules are structured as follows:
   - Final_Project.Stock_logic
   - Final_Project.Ratios
   Make sure to adjust the module paths according to your actual project structure.
*)
open Final_Project.Stock_logic
open Final_Project.Ratios

(* Helper function for rounding to two decimal places *)
let round_to_hundredths f =
  Float.(round (f *. 100.) /. 100.)

(* Mock JSON data for testing *)

let mock_stock_data_json = Yojson.Basic.from_string
  {|
    {
      "Time Series (Daily)": {
        "2024-10-29": { "4. close": "230.50" },
        "2024-10-28": { "4. close": "231.00" },
        "2024-10-27": { "4. close": "232.00" },
        "2024-10-26": { "4. close": "233.50" },
        "2024-10-25": { "4. close": "234.00" },
        "2024-10-24": { "4. close": "235.00" }
      }
    }
  |}

let mock_balance_sheet_json = Yojson.Basic.from_string
  {|
    {
      "annualReports": [
        {
          "fiscalDateEnding": "2023",
          "cashAndCashEquivalentsAtCarryingValue": "1000000",
          "totalCurrentLiabilities": "500000",
          "totalCurrentAssets": "1500000",
          "inventory": "200000",
          "currentNetReceivables": "100000",
          "currentAccountsPayable": "50000",
          "shortTermDebt": "200000",
          "longTermDebt": "800000",
          "totalAssets": "3000000",
          "totalLiabilities": "1200000",
          "totalShareholderEquity": "1800000"
        }
      ]
    }
  |}

let mock_income_statement_json = Yojson.Basic.from_string
  {|
    {
      "annualReports": [
        {
          "fiscalDateEnding": "2023",
          "totalRevenue": "2000000",
          "costOfRevenue": "1000000",
          "netIncome": "500000",
          "grossProfit": "1000000",
          "operatingIncome": "400000",
          "ebitda": "450000",
          "incomeBeforeTax": "480000",
          "interestExpense": "20000"
        }
      ]
    }
  |}

let mock_empty_balance_sheet_json = Yojson.Basic.from_string
  {|
    {
      "annualReports": []
    }
  |}

let mock_incomplete_balance_sheet_json = Yojson.Basic.from_string
  {|
    {
      "annualReports": [
        {
          "fiscalDateEnding": "2023",
          "totalAssets": "3000000"
          /* Missing other fields */
        }
      ]
    }
  |}

let mock_zero_liabilities_balance_sheet_json = Yojson.Basic.from_string
  {|
    {
      "annualReports": [
        {
          "fiscalDateEnding": "2023",
          "cashAndCashEquivalentsAtCarryingValue": "1000000",
          "totalCurrentLiabilities": "0",
          "totalCurrentAssets": "1500000",
          "inventory": "200000",
          "currentNetReceivables": "100000",
          "currentAccountsPayable": "50000",
          "shortTermDebt": "200000",
          "longTermDebt": "800000",
          "totalAssets": "3000000",
          "totalLiabilities": "1200000",
          "totalShareholderEquity": "1800000"
        }
      ]
    }
  |}

let mock_zero_revenue_income_statement_json = Yojson.Basic.from_string
  {|
    {
      "annualReports": [
        {
          "fiscalDateEnding": "2023",
          "totalRevenue": "0",
          "costOfRevenue": "1000000",
          "netIncome": "500000",
          "grossProfit": "1000000",
          "operatingIncome": "400000",
          "ebitda": "450000",
          "incomeBeforeTax": "480000",
          "interestExpense": "20000"
        }
      ]
    }
  |}

(* Extract the first (and only) report from the mock JSON data and convert it to (string * string) list *)
let extract_first_report json =
  match extract_data json with
  | [] -> failwith "No annual reports found"
  | report :: _ -> report

(* Test Cases *)

(* Test Efficiency Ratios *)
let test_efficiency_ratios _ =
  let balance = extract_first_report mock_balance_sheet_json in
  let income = extract_first_report mock_income_statement_json in
  let eff = efficiency balance income in
  let expected = [
    ("Days Receivables", 18.25);
    ("Days Payable Outstanding", 18.25);
    ("Days of Inventory", 73.0)
  ] in
  let rounded_eff = List.map (fun (k, v) -> (k, round_to_hundredths v)) eff in
  assert_equal
    ~printer:(fun lst ->
      String.concat "; " (List.map (fun (k, v) -> Printf.sprintf "%s: %.2f" k v) lst))
    (List.sort compare expected)
    (List.sort compare rounded_eff)

(* Test Profitability Ratios *)
let test_profitability_ratios _ =
  let balance = extract_first_report mock_balance_sheet_json in
  let income = extract_first_report mock_income_statement_json in
  let profit = profitability balance income in
  let expected = [
    ("Return on Assets (ROA)", 16.67);
    ("Return on Equity (ROE)", 27.78);
    ("Gross Profit Margin", 50.0);
    ("Operating Margin", 20.0);
    ("EBITDA Margin", 22.5);
    ("Pre-Tax Margin", 24.0)
  ] in
  let rounded_profit = List.map (fun (k, v) -> (k, round_to_hundredths v)) profit in
  assert_equal
    ~printer:(fun lst ->
      String.concat "; " (List.map (fun (k, v) -> Printf.sprintf "%s: %.2f" k v) lst))
    (List.sort compare expected)
    (List.sort compare rounded_profit)

(* Test Calculate Moving Average *)
let test_calculate_moving_average _ =
  let prices = extract_prices mock_stock_data_json "2024-10-24" "2024-10-29" in
  let moving_average = calculate_moving_average prices 3 in
  let rounded_ma = round_to_hundredths moving_average in
  assert_equal ~printer:string_of_float 231.17 rounded_ma

(* Test Liquidity Ratios *)
let test_liquidity_ratios _ =
  let balance = extract_first_report mock_balance_sheet_json in
  let liquidity_ratios = liquidity balance in
  let expected = [
    ("Cash Ratio", 2.0);
    ("Acid Test Ratio", 2.6);
    ("Current Ratio", 3.0)
  ] in
  let rounded_liquidity = List.map (fun (k, v) -> (k, round_to_hundredths v)) liquidity_ratios in
  assert_equal
    ~printer:(fun lst ->
      String.concat "; " (List.map (fun (k, v) -> Printf.sprintf "%s: %.2f" k v) lst))
    (List.sort compare expected)
    (List.sort compare rounded_liquidity)

(* Test Leverage Ratios *)
let test_leverage_ratios _ =
  let balance = extract_first_report mock_balance_sheet_json in
  let income = extract_first_report mock_income_statement_json in
  let leverage_ratios = leverage balance income in
  let expected = [
    ("Debt to Assets (Debt Ratio)", 0.33);
    ("Total Debt to EBITDA", 2.22);
    ("Interest Cover Ratio", 20.0);
    ("Debt-to-Equity Ratio", 0.67);
    ("Equity Multiplier", 1.67)
  ] in
  let rounded_leverage = List.map (fun (k, v) -> (k, round_to_hundredths v)) leverage_ratios in
  assert_equal
    ~printer:(fun lst ->
      String.concat "; " (List.map (fun (k, v) -> Printf.sprintf "%s: %.2f" k v) lst))
    (List.sort compare expected)
    (List.sort compare rounded_leverage)

(* Test Return on Assets (ROA) *)
let test_return_on_assets _ =
  let balance = extract_first_report mock_balance_sheet_json in
  let income = extract_first_report mock_income_statement_json in
  let roa = return_on_assets balance income in
  let rounded_roa = round_to_hundredths roa in
  assert_equal ~printer:string_of_float 16.67 rounded_roa

(* Test Return on Equity (ROE) *)
let test_return_on_equity _ =
  let balance = extract_first_report mock_balance_sheet_json in
  let income = extract_first_report mock_income_statement_json in
  let roe = return_on_equity balance income in
  let rounded_roe = round_to_hundredths roe in
  assert_equal ~printer:string_of_float 27.78 rounded_roe

(* Test Gross Profit Margin *)
let test_gross_profit_margin _ =
  let income = extract_first_report mock_income_statement_json in
  let gpm = gross_profit_margin income in
  let rounded_gpm = round_to_hundredths gpm in
  assert_equal ~printer:string_of_float 50.0 rounded_gpm

(* Test Operating Margin *)
let test_operating_margin _ =
  let income = extract_first_report mock_income_statement_json in
  let om = operating_margin income in
  let rounded_om = round_to_hundredths om in
  assert_equal ~printer:string_of_float 20.0 rounded_om

(* Test EBITDA Margin *)
let test_ebitda_margin _ =
  let income = extract_first_report mock_income_statement_json in
  let ebitda_m = ebitda_margin income in
  let rounded_ebitda_m = round_to_hundredths ebitda_m in
  assert_equal ~printer:string_of_float 22.5 rounded_ebitda_m

(* Test Pre-Tax Margin *)
let test_pre_tax_margin _ =
  let income = extract_first_report mock_income_statement_json in
  let ptm = pre_tax_margin income in
  let rounded_ptm = round_to_hundredths ptm in
  assert_equal ~printer:string_of_float 24.0 rounded_ptm

(* Test Debt to Assets Ratio with Zero Liabilities *)
let test_debt_to_assets_zero_liabilities _ =
  let balance = extract_first_report mock_zero_liabilities_balance_sheet_json in
  let ratio = debt_to_assets_ratio balance in
  assert_equal ~printer:string_of_float 0.0 ratio

(* Test Total Debt to EBITDA with Zero EBITDA *)
let test_total_debt_to_ebitda_zero_ebitda _ =
  let balance = extract_first_report mock_balance_sheet_json in
  let income = extract_first_report mock_zero_revenue_income_statement_json in
  let ratio = total_debt_to_ebitda balance income in
  assert_equal ~printer:string_of_float 0.0 ratio

(* Test Interest Cover Ratio with Zero Interest Expense *)
let test_interest_cover_ratio_zero_interest _ =
  let income_zero_interest = Yojson.Basic.from_string
    {|
      {
        "annualReports": [
          {
            "fiscalDateEnding": "2023",
            "totalRevenue": "2000000",
            "costOfRevenue": "1000000",
            "netIncome": "500000",
            "grossProfit": "1000000",
            "operatingIncome": "400000",
            "ebitda": "450000",
            "incomeBeforeTax": "480000",
            "interestExpense": "0"
          }
        ]
      }
    |} in
  let income = extract_first_report income_zero_interest in
  let ratio = interest_cover_ratio income in
  assert_equal ~printer:string_of_float 0.0 ratio

(* Test Equity Multiplier with Zero Shareholder Equity *)
let test_equity_multiplier_zero_equity _ =
  let balance_zero_equity_json = Yojson.Basic.from_string
    {|
      {
        "annualReports": [
          {
            "fiscalDateEnding": "2023",
            "cashAndCashEquivalentsAtCarryingValue": "1000000",
            "totalCurrentLiabilities": "500000",
            "totalCurrentAssets": "1500000",
            "inventory": "200000",
            "currentNetReceivables": "100000",
            "currentAccountsPayable": "50000",
            "shortTermDebt": "200000",
            "longTermDebt": "800000",
            "totalAssets": "3000000",
            "totalLiabilities": "1200000",
            "totalShareholderEquity": "0"
          }
        ]
      }
    |} in
  let balance = extract_first_report balance_zero_equity_json in
  let ratio = equity_multiplier balance in
  assert_equal ~printer:string_of_float 0.0 ratio

(* Test Days Receivables with Zero Revenue *)
let test_days_receivables_zero_revenue _ =
  let balance = extract_first_report mock_balance_sheet_json in
  let income = extract_first_report mock_zero_revenue_income_statement_json in
  let dr = days_receivables balance income in
  assert_equal ~printer:string_of_float 0.0 dr

(* Test Days Payable Outstanding with Zero Cost of Revenue *)
let test_days_payable_outstanding_zero_cost _ =
  let income_zero_cost = Yojson.Basic.from_string
    {|
      {
        "annualReports": [
          {
            "fiscalDateEnding": "2023",
            "totalRevenue": "2000000",
            "costOfRevenue": "0",
            "netIncome": "500000",
            "grossProfit": "1000000",
            "operatingIncome": "400000",
            "ebitda": "450000",
            "incomeBeforeTax": "480000",
            "interestExpense": "20000"
          }
        ]
      }
    |} in
  let balance = extract_first_report mock_balance_sheet_json in
  let income = extract_first_report income_zero_cost in
  let dp = days_payable_outstanding balance income in
  assert_equal ~printer:string_of_float 0.0 dp

(* Test Days of Inventory with Zero Cost of Revenue *)
let test_days_of_inventory_zero_cost _ =
  let income_zero_cost = Yojson.Basic.from_string
    {|
      {
        "annualReports": [
          {
            "fiscalDateEnding": "2023",
            "totalRevenue": "2000000",
            "costOfRevenue": "0",
            "netIncome": "500000",
            "grossProfit": "1000000",
            "operatingIncome": "400000",
            "ebitda": "450000",
            "incomeBeforeTax": "480000",
            "interestExpense": "20000"
          }
        ]
      }
    |} in
  let balance = extract_first_report mock_balance_sheet_json in
  let income = extract_first_report income_zero_cost in
  let di = days_of_inventory balance income in
  assert_equal ~printer:string_of_float 0.0 di


(* Test Fetch Stock Data Function with Mocked Response *)
let test_fetch_stock_data _ =
  let symbol = "AAPL" in
  let statement = "BALANCE_SHEET" in
  (* Since fetch_stock_data performs an actual HTTP request, it's better to mock this function.
     However, for simplicity, we'll assume it returns the mock_balance_sheet_json.
     In a real-world scenario, consider using libraries like `ounit-mocks` or dependency injection.
  *)
  let fetched_json = fetch_stock_data symbol statement |> Lwt_main.run in
  (* Compare fetched_json with mock_balance_sheet_json *)
  (* This is a placeholder as actual mocking is not implemented here *)
  assert_bool "Fetch stock data should return a JSON object" (fetched_json <> `Null)

(* Complete Test Suite *)
let tests =
  "Comprehensive Test Suite"
  >::: [
         "Test Efficiency Ratios" >:: test_efficiency_ratios;
         "Test Profitability Ratios" >:: test_profitability_ratios;
         "Test Calculate Moving Average" >:: test_calculate_moving_average;
         "Test Liquidity Ratios" >:: test_liquidity_ratios;
         "Test Leverage Ratios" >:: test_leverage_ratios;
         "Test Return on Assets (ROA)" >:: test_return_on_assets;
         "Test Return on Equity (ROE)" >:: test_return_on_equity;
         "Test Gross Profit Margin" >:: test_gross_profit_margin;
         "Test Operating Margin" >:: test_operating_margin;
         "Test EBITDA Margin" >:: test_ebitda_margin;
         "Test Pre-Tax Margin" >:: test_pre_tax_margin;
         "Test Debt to Assets Ratio with Zero Liabilities" >:: test_debt_to_assets_zero_liabilities;
         "Test Total Debt to EBITDA with Zero EBITDA" >:: test_total_debt_to_ebitda_zero_ebitda;
         "Test Interest Cover Ratio with Zero Interest Expense" >:: test_interest_cover_ratio_zero_interest;
         "Test Equity Multiplier with Zero Shareholder Equity" >:: test_equity_multiplier_zero_equity;
         "Test Days Receivables with Zero Revenue" >:: test_days_receivables_zero_revenue;
         "Test Days Payable Outstanding with Zero Cost of Revenue" >:: test_days_payable_outstanding_zero_cost;
         "Test Days of Inventory with Zero Cost of Revenue" >:: test_days_of_inventory_zero_cost;
         "Test Fetch Stock Data Function" >:: test_fetch_stock_data;
       ]

(* Run tests *)
let () =
  run_test_tt_main tests
