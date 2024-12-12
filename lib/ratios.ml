open Lwt.Infix
open Api_client

let safe_float_of_get_val report key =
  match get_val report key with
  | "None" -> 0.0
  | value -> ( try float_of_string value with Failure _ -> 0.0)

let cash_ratio balance =
  try
    let cashAndCashEquivalentsAtCarryingValue =
      safe_float_of_get_val balance "cashAndCashEquivalentsAtCarryingValue"
    in
    let totalCurrentLiabilities =
      safe_float_of_get_val balance "totalCurrentLiabilities"
    in
    if totalCurrentLiabilities = 0.0 then failwith "Division by zero"
    else cashAndCashEquivalentsAtCarryingValue /. totalCurrentLiabilities
  with
  | Failure msg ->
      print_endline ("Error: " ^ msg);
      0.0


let acid_test_ratio balance =
  try
    let totalCurrentAssets =
      safe_float_of_get_val balance "totalCurrentAssets"
    in
    let inventory = safe_float_of_get_val balance "inventory"
    in
    let totalCurrentLiabilities =
      safe_float_of_get_val balance "totalCurrentLiabilities"
    in
    if totalCurrentLiabilities = 0.0 then failwith "Division by zero"
    else (totalCurrentAssets -. inventory) /. totalCurrentLiabilities
  with
  | Failure msg ->
      print_endline ("Error: " ^ msg);
      0.0


let current_ratio balance =
  try
    let totalCurrentAssets =
      safe_float_of_get_val balance "totalCurrentAssets"
    in
    let totalCurrentLiabilities =
      safe_float_of_get_val balance "totalCurrentLiabilities"
    in
    if totalCurrentLiabilities = 0.0 then failwith "Division by zero"
    else totalCurrentAssets /. totalCurrentLiabilities
  with
  | Failure msg ->
      print_endline ("Error: " ^ msg);
      0.0


let liquidity balance =
  let liq_list = [] in
  let liq_list = ("Cash Ratio", cash_ratio balance) :: liq_list in
  let liq_list = ("Acid Test Ratio", acid_test_ratio balance) :: liq_list in
  let liq_list = ("Current Ratio", current_ratio balance) :: liq_list in
  liq_list

let days_receivables balance income =
  try
    let currentNetReceivables =
      safe_float_of_get_val balance "currentNetReceivables"
    in
    let totalRevenue = safe_float_of_get_val income "totalRevenue" in
    if totalRevenue = 0.0 then 0.0
    else currentNetReceivables /. totalRevenue *. 365.0
  with
  | Failure msg ->
      print_endline ("Error in days_receivables: " ^ msg);
      0.0


let days_payable_outstanding balance income =
  try
    let currentAccountsPayable =
      safe_float_of_get_val balance "currentAccountsPayable"
    in
    let costOfRevenue = safe_float_of_get_val income "costOfRevenue" in
    if costOfRevenue = 0.0 then 0.0
    else currentAccountsPayable /. costOfRevenue *. 365.0
  with
  | Failure msg ->
      print_endline ("Error in days_payable_outstanding: " ^ msg);
      0.0


let days_of_inventory balance income =
  try
    let inventory =
      let inv = get_val balance "inventory" in
      if inv = "None" then 0.0 else float_of_string inv
    in
    let costOfRevenue = safe_float_of_get_val income "costOfRevenue" in
    if costOfRevenue = 0.0 then 0.0 else inventory /. costOfRevenue *. 365.0
  with
  | Failure msg ->
      print_endline ("Error in days_of_inventory: " ^ msg);
      0.0


let efficiency balance income =
  let op_list = [] in
  let op_list =
    ("Days Receivables", days_receivables balance income) :: op_list
  in
  let op_list =
    ("Days Payable Outstanding", days_payable_outstanding balance income)
    :: op_list
  in
  let op_list =
    ("Days of Inventory", days_of_inventory balance income) :: op_list
  in
  op_list

let debt_to_assets_ratio balance =
  try
    let shortTermDebt = safe_float_of_get_val balance "shortTermDebt" in
    let longTermDebt =
      let ltd = get_val balance "longTermDebt" in
      if ltd = "None" then 0.0 else float_of_string ltd
    in
    let totalAssets = safe_float_of_get_val balance "totalAssets" in
    if totalAssets = 0.0 then 0.0
    else (shortTermDebt +. longTermDebt) /. totalAssets
  with
  | Failure msg ->
      print_endline ("Error in debt_to_assets_ratio: " ^ msg);
      0.0
 

let total_debt_to_ebitda balance income =
  try
    let shortTermDebt = safe_float_of_get_val balance "shortTermDebt" in
    let longTermDebt = safe_float_of_get_val balance "longTermDebt" in
    let ebitda = safe_float_of_get_val income "ebitda" in
    if ebitda = 0.0 then 0.0 else (shortTermDebt +. longTermDebt) /. ebitda
  with
  | Failure msg ->
      print_endline ("Error in total_debt_to_ebitda: " ^ msg);
      0.0
  

let interest_cover_ratio income =
  try
    let ebit = safe_float_of_get_val income "ebit" in
    let interestExpense = safe_float_of_get_val income "interestExpense"
    in
    if interestExpense = 0.0 then 0.0 else ebit /. interestExpense
  with
  | Failure msg ->
      print_endline ("Error in interest_cover_ratio: " ^ msg);
      0.0


let debt_to_equity_ratio balance =
  try
    let totalLiabilities = safe_float_of_get_val balance "totalLiabilities" in
    let totalShareholderEquity =
      safe_float_of_get_val balance "totalShareholderEquity"
    in
    if totalShareholderEquity = 0.0 then 0.0
    else totalLiabilities /. totalShareholderEquity
  with
  | Failure msg ->
      print_endline ("Error in debt_to_equity_ratio: " ^ msg);
      0.0


let equity_multiplier balance =
  try
    let totalAssets = safe_float_of_get_val balance "totalAssets" in
    let totalShareholderEquity =
      safe_float_of_get_val balance "totalShareholderEquity"
    in
    if totalShareholderEquity = 0.0 then 0.0
    else totalAssets /. totalShareholderEquity
  with
  | Failure msg ->
      print_endline ("Error in equity_multiplier: " ^ msg);
      0.0


let leverage balance income =
  let fin_list = [] in
  let fin_list =
    ("Debt to Assets (Debt Ratio)", debt_to_assets_ratio balance) :: fin_list
  in
  let fin_list =
    ("Total Debt to EBITDA", total_debt_to_ebitda balance income) :: fin_list
  in
  let fin_list =
    ("Interest Cover Ratio", interest_cover_ratio income) :: fin_list
  in
  let fin_list =
    ("Debt-to-Equity Ratio", debt_to_equity_ratio balance) :: fin_list
  in
  let fin_list = ("Equity Multiplier", equity_multiplier balance) :: fin_list in
  fin_list

let return_on_assets balance income =
  try
    let netIncome = safe_float_of_get_val income "netIncome" in
    let totalAssets = safe_float_of_get_val balance "totalAssets" in
    if totalAssets = 0.0 then 0.0 else netIncome /. totalAssets *. 100.0
  with
  | Failure msg ->
      print_endline ("Error in return_on_assets: " ^ msg);
      0.0
 

let return_on_equity balance income =
  try
    let netIncome = safe_float_of_get_val income "netIncome" in
    let totalShareholderEquity =
      safe_float_of_get_val balance "totalShareholderEquity"
    in
    if totalShareholderEquity = 0.0 then 0.0
    else netIncome /. totalShareholderEquity *. 100.0
  with
  | Failure msg ->
      print_endline ("Error in return_on_equity: " ^ msg);
      0.0


let gross_profit_margin income =
  try
    let grossProfit = safe_float_of_get_val income "grossProfit" in
    let totalRevenue = safe_float_of_get_val income "totalRevenue" in
    if totalRevenue = 0.0 then 0.0 else grossProfit /. totalRevenue *. 100.0
  with
  | Failure msg ->
      print_endline ("Error in gross_profit_margin: " ^ msg);
      0.0


let operating_margin income =
  try
    let operatingIncome = safe_float_of_get_val income "operatingIncome" in
    let totalRevenue = safe_float_of_get_val income "totalRevenue" in

    if totalRevenue = 0.0 then 0.0 else operatingIncome /. totalRevenue *. 100.0
  with
  | Failure msg ->
      print_endline ("Error in operating_margin: " ^ msg);
      0.0


let ebitda_margin income =
  try
    let ebitda = safe_float_of_get_val income "ebitda" in
    let totalRevenue = safe_float_of_get_val income "totalRevenue" in
    if totalRevenue = 0.0 then 0.0 else ebitda /. totalRevenue *. 100.0
  with
  | Failure msg ->
      print_endline ("Error in ebitda_margin: " ^ msg);
      0.0
 

let pre_tax_margin income =
  try
    let incomeBeforeTax = safe_float_of_get_val income "incomeBeforeTax" in
    let totalRevenue = safe_float_of_get_val income "totalRevenue" in
    if totalRevenue = 0.0 then 0.0 else incomeBeforeTax /. totalRevenue *. 100.0
  with
  | Failure msg ->
      print_endline ("Error in pre_tax_margin: " ^ msg);
      0.0


let profitability balance income =
  let profit_list = [] in
  let profit_list =
    ("Return on Assets (ROA)", return_on_assets balance income) :: profit_list
  in
  let profit_list =
    ("Return on Equity (ROE)", return_on_equity balance income) :: profit_list
  in
  let profit_list =
    ("Gross Profit Margin", gross_profit_margin income) :: profit_list
  in
  let profit_list =
    ("Operating Margin", operating_margin income) :: profit_list
  in
  let profit_list = ("EBITDA Margin", ebitda_margin income) :: profit_list in
  let profit_list = ("Pre-Tax Margin", pre_tax_margin income) :: profit_list in

  profit_list
