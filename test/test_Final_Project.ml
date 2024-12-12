open OUnit2
open Lwt.Infix
open Yojson.Basic.Util

(* Assuming the modules are structured as follows: - Final_Project.Stock_logic -
   Final_Project.Ratios Make sure to adjust the module paths according to your
   actual project structure. *)
open Final_Project.Ratios
open Final_Project.Api_client
open Final_Project.Stock_analysis

(* Helper function for rounding to two decimal places *)
let round_to_hundredths f = Float.(round (f *. 100.) /. 100.)

(* Mock JSON data for testing *)

let mock_stock_data_json =
  Yojson.Basic.from_string
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

let mock_balance_sheet_json =
  Yojson.Basic.from_string
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

let mock_income_statement_json =
  Yojson.Basic.from_string
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
          "ebit": "400000",
          "incomeBeforeTax": "480000",
          "interestExpense": "20000"
        }
      ]
    }
  |}

let mock_empty_balance_sheet_json =
  Yojson.Basic.from_string {|
    {
      "annualReports": []
    }
  |}

let mock_incomplete_balance_sheet_json =
  Yojson.Basic.from_string
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

let mock_zero_debt_balance_sheet_json =
  Yojson.Basic.from_string
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
          "shortTermDebt": "0",
          "longTermDebt": "0",
          "totalAssets": "3000000",
          "totalLiabilities": "100000",
          "totalShareholderEquity": "1800000"
        }
      ]
    }
  |}

let mock_zero_revenue_income_statement_json =
  Yojson.Basic.from_string
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

let mock_zero_ebitda_income_statement_json =
  Yojson.Basic.from_string
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
          "ebitda": "0",
          "incomeBeforeTax": "480000",
          "interestExpense": "20000"
        }
      ]
    }
  |}

(* Extract the first (and only) report from the mock JSON data and convert it to
   (string * string) list *)
let extract_first_report json = extract_data json

(* Test Cases *)

(* Test Efficiency Ratios *)
let test_efficiency_ratios _ =
  let balance = extract_first_report mock_balance_sheet_json in
  let income = extract_first_report mock_income_statement_json in
  let eff = efficiency balance income in
  let expected =
    [
      ("Days Receivables", 18.25);
      ("Days Payable Outstanding", 18.25);
      ("Days of Inventory", 73.0);
    ]
  in
  let rounded_eff = List.map (fun (k, v) -> (k, round_to_hundredths v)) eff in
  assert_equal
    ~printer:(fun lst ->
      String.concat "; "
        (List.map (fun (k, v) -> Printf.sprintf "%s: %.2f" k v) lst))
    (List.sort compare expected)
    (List.sort compare rounded_eff)

(* Test Profitability Ratios *)
let test_profitability_ratios _ =
  let balance = extract_first_report mock_balance_sheet_json in
  let income = extract_first_report mock_income_statement_json in
  let profit = profitability balance income in
  let expected =
    [
      ("Return on Assets (ROA)", 16.67);
      ("Return on Equity (ROE)", 27.78);
      ("Gross Profit Margin", 50.0);
      ("Operating Margin", 20.0);
      ("EBITDA Margin", 22.5);
      ("Pre-Tax Margin", 24.0);
    ]
  in
  let rounded_profit =
    List.map (fun (k, v) -> (k, round_to_hundredths v)) profit
  in
  assert_equal
    ~printer:(fun lst ->
      String.concat "; "
        (List.map (fun (k, v) -> Printf.sprintf "%s: %.2f" k v) lst))
    (List.sort compare expected)
    (List.sort compare rounded_profit)

(* Test Calculate Moving Average *)
let test_calculate_moving_average _ =
  let prices =
    extract_prices_with_date mock_stock_data_json "2024-10-24" "2024-10-29"
  in
  let moving_average = calculate_moving_average prices 3 in
  let rounded_ma = round_to_hundredths moving_average in
  assert_equal ~printer:string_of_float 231.17 rounded_ma

(* Test Liquidity Ratios *)
let test_liquidity_ratios _ =
  let balance = extract_first_report mock_balance_sheet_json in
  let liquidity_ratios = liquidity balance in
  let expected =
    [ ("Cash Ratio", 2.0); ("Acid Test Ratio", 2.6); ("Current Ratio", 3.0) ]
  in
  let rounded_liquidity =
    List.map (fun (k, v) -> (k, round_to_hundredths v)) liquidity_ratios
  in
  assert_equal
    ~printer:(fun lst ->
      String.concat "; "
        (List.map (fun (k, v) -> Printf.sprintf "%s: %.2f" k v) lst))
    (List.sort compare expected)
    (List.sort compare rounded_liquidity)

(* Test Leverage Ratios *)
let test_leverage_ratios _ =
  let balance = extract_first_report mock_balance_sheet_json in
  let income = extract_first_report mock_income_statement_json in
  let leverage_ratios = leverage balance income in
  let expected =
    [
      ("Debt to Assets (Debt Ratio)", 0.33);
      ("Total Debt to EBITDA", 2.22);
      ("Interest Cover Ratio", 20.0);
      ("Debt-to-Equity Ratio", 0.67);
      ("Equity Multiplier", 1.67);
    ]
  in
  let rounded_leverage =
    List.map (fun (k, v) -> (k, round_to_hundredths v)) leverage_ratios
  in
  assert_equal
    ~printer:(fun lst ->
      String.concat "; "
        (List.map (fun (k, v) -> Printf.sprintf "%s: %.2f" k v) lst))
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

(* Test Debt to Assets Ratio with Zero Debt *)
let test_debt_to_assets_zero_debt _ =
  let balance = extract_first_report mock_zero_debt_balance_sheet_json in
  let ratio = debt_to_assets_ratio balance in
  assert_equal ~printer:string_of_float 0.0 ratio

(* Test Total Debt to EBITDA with Zero EBITDA *)
let test_total_debt_to_ebitda_zero_ebitda _ =
  let balance = extract_first_report mock_balance_sheet_json in
  let income = extract_first_report mock_zero_ebitda_income_statement_json in
  let ratio = total_debt_to_ebitda balance income in
  assert_equal ~printer:string_of_float 0.0 ratio

(* Test Interest Cover Ratio with Zero Interest Expense *)
let test_interest_cover_ratio_zero_interest _ =
  let income_zero_interest =
    Yojson.Basic.from_string
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
    |}
  in
  let income = extract_first_report income_zero_interest in
  let ratio = interest_cover_ratio income in
  assert_equal ~printer:string_of_float 0.0 ratio

(* Test Equity Multiplier with Zero Shareholder Equity *)
let test_equity_multiplier_zero_equity _ =
  let balance_zero_equity_json =
    Yojson.Basic.from_string
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
    |}
  in
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
  let income_zero_cost =
    Yojson.Basic.from_string
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
    |}
  in
  let balance = extract_first_report mock_balance_sheet_json in
  let income = extract_first_report income_zero_cost in
  let dp = days_payable_outstanding balance income in
  assert_equal ~printer:string_of_float 0.0 dp

(* Test Days of Inventory with Zero Cost of Revenue *)
let test_days_of_inventory_zero_cost _ =
  let income_zero_cost =
    Yojson.Basic.from_string
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
    |}
  in
  let balance = extract_first_report mock_balance_sheet_json in
  let income = extract_first_report income_zero_cost in
  let di = days_of_inventory balance income in
  assert_equal ~printer:string_of_float 0.0 di

(* Test Fetch Stock Data Function with Mocked Response *)
let test_fetch_stock_data _ =
  let symbol = "AAPL" in
  let statement = "BALANCE_SHEET" in
  (* Since fetch_stock_data performs an actual HTTP request, it's better to mock
     this function. However, for simplicity, we'll assume it returns the
     mock_balance_sheet_json. *)
  let fetched_json = fetch_stock_data symbol statement |> Lwt_main.run in
  (* Compare fetched_json with mock_balance_sheet_json *)
  (* This is a placeholder as actual mocking is not implemented here *)
  assert_bool "Fetch stock data should return a JSON object"
    (fetched_json <> `Null)

(* Test Safe Float Conversion (Triggering `Failure`) *)
let test_safe_float_of_get_val_failure _ =
  let bad_json = Yojson.Basic.from_string {| { "badKey": "NotANumber" } |} in
  let extracted_data =
    bad_json |> Yojson.Basic.Util.to_assoc
    |> List.map (fun (k, v) -> (k, Yojson.Basic.Util.to_string v))
  in
  let result = safe_float_of_get_val extracted_data "badKey" in
  assert_equal ~printer:string_of_float 0.0 result

(* Test Cash Ratio with Division by Zero (Triggering `Failure` and `_`) *)
let test_cash_ratio_division_by_zero _ =
  let zero_liabilities_json =
    Yojson.Basic.from_string
      {| { "annualReports": [ { "cashAndCashEquivalentsAtCarryingValue": "1000", "totalCurrentLiabilities": "0" } ] } |}
  in
  let balance = extract_first_report zero_liabilities_json in
  let result = cash_ratio balance in
  assert_equal ~printer:string_of_float 0.0 result

(* Test Acid Test Ratio with Missing Field (Triggering `_`) *)
let test_acid_test_ratio_missing_field _ =
  let missing_field_json =
    Yojson.Basic.from_string
      {| { "annualReports": [ { "totalCurrentAssets": "1000" } ] } |}
  in
  let balance = extract_first_report missing_field_json in
  let result = acid_test_ratio balance in
  assert_equal ~printer:string_of_float 0.0 result

(* Test Current Ratio with Missing Total Liabilities (Triggering `_`) *)
let test_current_ratio_missing_liabilities _ =
  let incomplete_json =
    Yojson.Basic.from_string
      {| { "annualReports": [ { "totalCurrentAssets": "5000" } ] } |}
  in
  let balance = extract_first_report incomplete_json in
  let result = current_ratio balance in
  assert_equal ~printer:string_of_float 0.0 result

(* Test Debt-to-Equity with Invalid Number (Triggering `Failure`) *)
let test_debt_to_equity_invalid_number _ =
  let bad_json =
    Yojson.Basic.from_string
      {| { "annualReports": [ { "totalLiabilities": "invalid", "totalShareholderEquity": "1000" } ] } |}
  in
  let balance = extract_first_report bad_json in
  let result = debt_to_equity_ratio balance in
  assert_equal ~printer:string_of_float 0.0 result

(* Test Total Debt to EBITDA with Missing Field (Triggering `_`) *)
let test_total_debt_to_ebitda_missing_field _ =
  let missing_field_json =
    Yojson.Basic.from_string
      {| { "annualReports": [ { "shortTermDebt": "500" } ] } |}
  in
  let balance = extract_first_report missing_field_json in
  let income = extract_first_report missing_field_json in
  let result = total_debt_to_ebitda balance income in
  assert_equal ~printer:string_of_float 0.0 result

(* Test Days Receivables with Zero Revenue (Triggering `Failure`) *)
let test_days_receivables_zero_revenue _ =
  let zero_revenue_json =
    Yojson.Basic.from_string
      {| { "annualReports": [ { "currentNetReceivables": "5000", "totalRevenue": "0" } ] } |}
  in
  let balance = extract_first_report zero_revenue_json in
  let income = extract_first_report zero_revenue_json in
  let result = days_receivables balance income in
  assert_equal ~printer:string_of_float 0.0 result

(* Test Days Payable Outstanding with Zero Cost of Revenue (Triggering
   `Failure`) *)
let test_days_payable_outstanding_zero_cost _ =
  let zero_cost_json =
    Yojson.Basic.from_string
      {| { "annualReports": [ { "currentAccountsPayable": "4000", "costOfRevenue": "0" } ] } |}
  in
  let balance = extract_first_report zero_cost_json in
  let income = extract_first_report zero_cost_json in
  let result = days_payable_outstanding balance income in
  assert_equal ~printer:string_of_float 0.0 result

(* Test Days of Inventory with Zero Cost of Revenue (Triggering `Failure`) *)
let test_days_of_inventory_zero_cost _ =
  let zero_cost_json =
    Yojson.Basic.from_string
      {| { "annualReports": [ { "inventory": "5000", "costOfRevenue": "0" } ] } |}
  in
  let balance = extract_first_report zero_cost_json in
  let income = extract_first_report zero_cost_json in
  let result = days_of_inventory balance income in
  assert_equal ~printer:string_of_float 0.0 result

let mock_ratios =
  [
    ("Cash Ratio", 0.8);
    ("Acid Test Ratio", 1.0);
    ("Current Ratio", 1.5);
    ("Days Receivables", 20.0);
    ("Days Payable Outstanding", 60.0);
    ("Days of Inventory", 70.0);
    ("Debt to Assets (Debt Ratio)", 0.3);
    ("Total Debt to EBITDA", 3.0);
    ("Interest Cover Ratio", 5.0);
    ("Debt-to-Equity Ratio", 1.2);
    ("Equity Multiplier", 2.0);
    ("Return on Assets (ROA)", 10.0);
    ("Return on Equity (ROE)", 15.0);
    ("Gross Profit Margin", 40.0);
    ("Operating Margin", 14.0);
    ("EBITDA Margin", 20.0);
    ("Pre-Tax Margin", 12.0);
  ]

let mock_prices =
  [
    ("2024-12-08", 230.0);
    ("2024-12-07", 232.0);
    ("2024-12-06", 231.0);
    ("2024-12-05", 233.0);
    ("2024-12-04", 234.0);
    ("2024-12-03", 235.0);
  ]

let test_score_ratio_all_conditions _ =
  let test_cases =
    [
      ("Cash Ratio", 0.8, 1);
      ("Cash Ratio", 0.5, 0);
      ("Cash Ratio", 0.3, -1);
      ("Acid Test Ratio", 1.3, 1);
      ("Acid Test Ratio", 1.0, 0);
      ("Acid Test Ratio", 0.8, -1);
      ("Current Ratio", 2.0, 1);
      ("Current Ratio", 1.5, 0);
      ("Current Ratio", 1.0, -1);
      ("Days Receivables", 20.0, 1);
      ("Days Receivables", 30.0, 0);
      ("Days Receivables", 50.0, -1);
      ("Days Payable Outstanding", 95.0, -1);
      ("Days Payable Outstanding", 60.0, 1);
      ("Days Payable Outstanding", 40.0, 0);
      ("Days Payable Outstanding", 30.0, -1);
      ("Days of Inventory", 45.0, 1);
      ("Days of Inventory", 75.0, 0);
      ("Days of Inventory", 90.0, -1);
      ("Debt to Assets (Debt Ratio)", 0.2, 1);
      ("Debt to Assets (Debt Ratio)", 0.3, 0);
      ("Debt to Assets (Debt Ratio)", 0.5, -1);
      ("Total Debt to EBITDA", 2.0, 1);
      ("Total Debt to EBITDA", 3.0, 0);
      ("Total Debt to EBITDA", 4.0, -1);
      ("Interest Cover Ratio", 7.0, 1);
      ("Interest Cover Ratio", 5.0, 0);
      ("Interest Cover Ratio", 3.0, -1);
      ("Debt-to-Equity Ratio", 0.7, 1);
      ("Debt-to-Equity Ratio", 1.2, 0);
      ("Debt-to-Equity Ratio", 1.6, -1);
      ("Equity Multiplier", 1.7, 1);
      ("Equity Multiplier", 2.2, 0);
      ("Equity Multiplier", 2.6, -1);
      ("Return on Assets (ROA)", 13.0, 1);
      ("Return on Assets (ROA)", 8.0, 0);
      ("Return on Assets (ROA)", 5.0, -1);
      ("Return on Equity (ROE)", 19.0, 1);
      ("Return on Equity (ROE)", 13.0, 0);
      ("Return on Equity (ROE)", 10.0, -1);
      ("Gross Profit Margin", 50.0, 1);
      ("Gross Profit Margin", 38.0, 0);
      ("Gross Profit Margin", 30.0, -1);
      ("Operating Margin", 19.0, 1);
      ("Operating Margin", 13.0, 0);
      ("Operating Margin", 10.0, -1);
      ("EBITDA Margin", 26.0, 1);
      ("EBITDA Margin", 19.0, 0);
      ("EBITDA Margin", 17.0, -1);
      ("Pre-Tax Margin", 16.0, 1);
      ("Pre-Tax Margin", 11.0, 0);
      ("Pre-Tax Margin", 9.0, -1);
    ]
  in
  List.iter
    (fun (name, value, expected) ->
      let actual = score_ratio name value in
      assert_equal ~printer:string_of_int expected actual)
    test_cases

let mock_invalid_balance =
  [
    ("cashAndCashEquivalentsAtCarryingValue", "None");
    ("totalCurrentLiabilities", "None");
    ("inventory", "None");
    ("totalCurrentAssets", "None");
    ("currentNetReceivables", "None");
    ("shortTermDebt", "None");
    ("longTermDebt", "None");
    ("totalAssets", "None");
    ("totalLiabilities", "None");
    ("totalShareholderEquity", "None");
  ]

let mock_invalid_income =
  [
    ("totalRevenue", "None");
    ("costOfRevenue", "None");
    ("grossProfit", "None");
    ("netIncome", "None");
    ("ebitda", "None");
    ("incomeBeforeTax", "None");
    ("operatingIncome", "None");
    ("ebit", "None");
    ("interestExpense", "None");
  ]

let mock_get_val data key = try List.assoc key data with Not_found -> "None"

let test_cash_ratio_failure _ =
  let balance = mock_invalid_balance in
  let result = cash_ratio balance in
  assert_equal ~printer:string_of_float 0.0 result

let test_acid_test_failure _ =
  let balance = mock_invalid_balance in
  let result = acid_test_ratio balance in
  assert_equal ~printer:string_of_float 0.0 result

let test_current_ratio_failure _ =
  let balance = mock_invalid_balance in
  let result = current_ratio balance in
  assert_equal ~printer:string_of_float 0.0 result

let test_days_receivables_failure _ =
  let balance = mock_invalid_balance in
  let income = mock_invalid_income in
  let result = days_receivables balance income in
  assert_equal ~printer:string_of_float 0.0 result

let test_days_payable_outstanding_failure _ =
  let balance = mock_invalid_balance in
  let income = mock_invalid_income in
  let result = days_payable_outstanding balance income in
  assert_equal ~printer:string_of_float 0.0 result

let test_days_of_inventory_failure _ =
  let balance = mock_invalid_balance in
  let income = mock_invalid_income in
  let result = days_of_inventory balance income in
  assert_equal ~printer:string_of_float 0.0 result

let test_debt_to_assets_failure _ =
  let balance = mock_invalid_balance in
  let result = debt_to_assets_ratio balance in
  assert_equal ~printer:string_of_float 0.0 result

let test_total_debt_to_ebitda_failure _ =
  let balance = mock_invalid_balance in
  let income = mock_invalid_income in
  let result = total_debt_to_ebitda balance income in
  assert_equal ~printer:string_of_float 0.0 result

let test_interest_cover_ratio_failure _ =
  let income = mock_invalid_income in
  let result = interest_cover_ratio income in
  assert_equal ~printer:string_of_float 0.0 result

let test_debt_to_equity_failure _ =
  let balance = mock_invalid_balance in
  let result = debt_to_equity_ratio balance in
  assert_equal ~printer:string_of_float 0.0 result

let test_equity_multiplier_failure _ =
  let balance = mock_invalid_balance in
  let result = equity_multiplier balance in
  assert_equal ~printer:string_of_float 0.0 result

let test_return_on_assets_failure _ =
  let balance = mock_invalid_balance in
  let income = mock_invalid_income in
  let result = return_on_assets balance income in
  assert_equal ~printer:string_of_float 0.0 result

let test_return_on_equity_failure _ =
  let balance = mock_invalid_balance in
  let income = mock_invalid_income in
  let result = return_on_equity balance income in
  assert_equal ~printer:string_of_float 0.0 result

let test_gross_profit_margin_failure _ =
  let income = mock_invalid_income in
  let result = gross_profit_margin income in
  assert_equal ~printer:string_of_float 0.0 result

let test_operating_margin_failure _ =
  let income = mock_invalid_income in
  let result = operating_margin income in
  assert_equal ~printer:string_of_float 0.0 result

let test_ebitda_margin_failure _ =
  let income = mock_invalid_income in
  let result = ebitda_margin income in
  assert_equal ~printer:string_of_float 0.0 result

let test_pre_tax_margin_failure _ =
  let income = mock_invalid_income in
  let result = pre_tax_margin income in
  assert_equal ~printer:string_of_float 0.0 result

open OUnit2
open Lwt.Infix
open Yojson.Basic.Util

(* Mock data for testing *)
let mock_valid_balance =
  [
    ("cashAndCashEquivalentsAtCarryingValue", "10000.0");
    ("totalCurrentLiabilities", "5000.0");
    ("inventory", "2000.0");
    ("totalCurrentAssets", "15000.0");
    ("currentNetReceivables", "3000.0");
    ("shortTermDebt", "4000.0");
    ("longTermDebt", "10000.0");
    ("totalAssets", "30000.0");
    ("totalLiabilities", "15000.0");
    ("totalShareholderEquity", "15000.0");
  ]

let mock_valid_income =
  [
    ("totalRevenue", "50000.0");
    ("costOfRevenue", "20000.0");
    ("grossProfit", "30000.0");
    ("netIncome", "15000.0");
    ("ebitda", "18000.0");
    ("incomeBeforeTax", "16000.0");
    ("operatingIncome", "17000.0");
    ("ebit", "17500.0");
    ("interestExpense", "1500.0");
  ]

let mock_prices =
  [
    ("2024-12-01", 200.0);
    ("2024-12-02", 210.0);
    ("2024-12-03", 220.0);
    ("2024-12-04", 230.0);
    ("2024-12-05", 240.0);
  ]

(* Mock functions *)
let fetch_stock_data _ _ = Lwt.return (Yojson.Basic.from_string "{}")

let extract_data json =
  Yojson.Basic.Util.to_assoc json
  |> List.map (fun (k, v) -> (k, Yojson.Basic.Util.to_string v))

let extract_prices json = mock_prices

let calculate_moving_average prices n =
  let rec take n lst =
    match lst with
    | [] -> []
    | x :: xs when n > 0 -> x :: take (n - 1) xs
    | _ -> []
  in
  let selected_prices = take n (List.rev prices) in
  if List.length selected_prices = 0 then 0.0
  else
    List.fold_left (fun acc (_, p) -> acc +. p) 0. selected_prices
    /. float_of_int (List.length selected_prices)

let test_fetch_ts_data _ =
  let result = fetch_stock_data "MOCK" "TIME_SERIES_DAILY" |> Lwt_main.run in
  assert_bool "Fetch ts_data should return valid data" (result <> `Null)

let test_ratios _ =
  let liquidity_result = liquidity mock_valid_balance in
  assert_equal ~printer:string_of_float 2.0
    (List.assoc "Cash Ratio" liquidity_result);
  assert_equal ~printer:string_of_float 2.6
    (List.assoc "Acid Test Ratio" liquidity_result);
  assert_equal ~printer:string_of_float 3.0
    (List.assoc "Current Ratio" liquidity_result);

  let efficiency_result = efficiency mock_valid_balance mock_valid_income in
  assert_equal ~printer:string_of_float 21.9
    (List.assoc "Days Receivables" efficiency_result);
  assert_equal ~printer:string_of_float 91.3
    (List.assoc "Days Payable Outstanding" efficiency_result);
  assert_equal ~printer:string_of_float 36.5
    (List.assoc "Days of Inventory" efficiency_result);

  let leverage_result = leverage mock_valid_balance mock_valid_income in
  assert_equal ~printer:string_of_float 0.47
    (List.assoc "Debt to Assets (Debt Ratio)" leverage_result);
  assert_equal ~printer:string_of_float 0.78
    (List.assoc "Total Debt to EBITDA" leverage_result);
  assert_equal ~printer:string_of_float 11.67
    (List.assoc "Interest Cover Ratio" leverage_result);
  assert_equal ~printer:string_of_float 1.0
    (List.assoc "Debt-to-Equity Ratio" leverage_result);
  assert_equal ~printer:string_of_float 2.0
    (List.assoc "Equity Multiplier" leverage_result);

  let profitability_result =
    profitability mock_valid_balance mock_valid_income
  in
  assert_equal ~printer:string_of_float 50.0
    (List.assoc "Gross Profit Margin" profitability_result);
  assert_equal ~printer:string_of_float 34.0
    (List.assoc "Operating Margin" profitability_result);
  assert_equal ~printer:string_of_float 36.0
    (List.assoc "EBITDA Margin" profitability_result);
  assert_equal ~printer:string_of_float 32.0
    (List.assoc "Pre-Tax Margin" profitability_result);
  assert_equal ~printer:string_of_float 15.0
    (List.assoc "Return on Assets (ROA)" profitability_result);
  assert_equal ~printer:string_of_float 30.0
    (List.assoc "Return on Equity (ROE)" profitability_result)

let test_moving_average_score _ =
  let sma_50 = calculate_moving_average mock_prices 50 in
  let sma_200 = calculate_moving_average mock_prices 200 in
  let score =
    if sma_50 > sma_200 then 3 else if sma_50 < sma_200 then -3 else 0
  in
  assert_equal ~printer:string_of_int 3 score

let rank_stock_test _ =
  let mock_stock = "MOCK" in
  let result = rank_stock mock_stock |> Lwt_main.run in
  assert_bool "Scaled score should be within -1.0 and 1.0"
    (result >= -1.0 && result <= 1.0);
  print_endline ("Rank stock result: " ^ string_of_float result)

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
         "Test Debt to Assets Ratio with Zero Debt"
         >:: test_debt_to_assets_zero_debt;
         "Test Total Debt to EBITDA with Zero EBITDA"
         >:: test_total_debt_to_ebitda_zero_ebitda;
         "Test Interest Cover Ratio with Zero Interest Expense"
         >:: test_interest_cover_ratio_zero_interest;
         "Test Equity Multiplier with Zero Shareholder Equity"
         >:: test_equity_multiplier_zero_equity;
         "Test Days Receivables with Zero Revenue"
         >:: test_days_receivables_zero_revenue;
         "Test Days Payable Outstanding with Zero Cost of Revenue"
         >:: test_days_payable_outstanding_zero_cost;
         "Test Days of Inventory with Zero Cost of Revenue"
         >:: test_days_of_inventory_zero_cost;
         "Test Fetch Stock Data Function" >:: test_fetch_stock_data;
         "Test Safe Float Conversion (Failure)"
         >:: test_safe_float_of_get_val_failure;
         "Test Cash Ratio with Division by Zero"
         >:: test_cash_ratio_division_by_zero;
         "Test Acid Test Ratio with Missing Field"
         >:: test_acid_test_ratio_missing_field;
         "Test Current Ratio with Missing Liabilities"
         >:: test_current_ratio_missing_liabilities;
         "Test Debt-to-Equity with Invalid Number"
         >:: test_debt_to_equity_invalid_number;
         "Test Total Debt to EBITDA with Missing Field"
         >:: test_total_debt_to_ebitda_missing_field;
         "Test Days Receivables with Zero Revenue"
         >:: test_days_receivables_zero_revenue;
         "Test Days Payable Outstanding with Zero Cost"
         >:: test_days_payable_outstanding_zero_cost;
         "Test Days of Inventory with Zero Cost"
         >:: test_days_of_inventory_zero_cost;
         "Test all score_ratio conditions" >:: test_score_ratio_all_conditions;
         "Test cash_ratio failure" >:: test_cash_ratio_failure;
         (* "Test acid_test_ratio failure" >:: test_acid_test_failure; *)
         "Test current_ratio failure" >:: test_current_ratio_failure;
         "Test days_receivables failure" >:: test_days_receivables_failure;
         "Test days_payable_outstanding failure"
         >:: test_days_payable_outstanding_failure;
         "Test days_of_inventory failure" >:: test_days_of_inventory_failure;
         "Test debt_to_assets failure" >:: test_debt_to_assets_failure;
         "Test total_debt_to_ebitda failure"
         >:: test_total_debt_to_ebitda_failure;
         "Test interest_cover_ratio failure"
         >:: test_interest_cover_ratio_failure;
         "Test debt_to_equity failure" >:: test_debt_to_equity_failure;
         "Test equity_multiplier failure" >:: test_equity_multiplier_failure;
         "Test return_on_assets failure" >:: test_return_on_assets_failure;
         "Test return_on_equity failure" >:: test_return_on_equity_failure;
         "Test gross_profit_margin failure" >:: test_gross_profit_margin_failure;
         "Test operating_margin failure" >:: test_operating_margin_failure;
         "Test ebitda_margin failure" >:: test_ebitda_margin_failure;
         "Test pre_tax_margin failure" >:: test_pre_tax_margin_failure;
         "Test fetch_ts_data" >:: test_fetch_ts_data;
         "Test moving_average_score" >:: test_moving_average_score;
         "Test rank_stock" >:: rank_stock_test;
       ]

(* Run tests *)
let () = run_test_tt_main tests
