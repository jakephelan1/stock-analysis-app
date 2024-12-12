open Lwt.Infix
open Api_client
open Ratios

let score_ratio name value =
  match name with
  | "Cash Ratio" -> if value > 0.7 then 1 else if value > 0.4 then 0 else -1
  | "Acid Test Ratio" ->
      if value > 1.2 then 1 else if value > 0.9 then 0 else -1
  | "Current Ratio" -> if value > 1.8 then 1 else if value > 1.3 then 0 else -1
  | "Days Receivables" ->
      if value < 25.0 then 1 else if value < 40.0 then 0 else -1
  | "Days Payable Outstanding" ->
      if value > 90.0 then -1
      else if value > 50.0 then 1
      else if value > 35.0 then 0
      else -1
  | "Days of Inventory" ->
      if value < 50.0 then 1 else if value < 80.0 then 0 else -1
  | "Debt to Assets (Debt Ratio)" ->
      if value < 0.25 then 1 else if value < 0.45 then 0 else -1
  | "Total Debt to EBITDA" ->
      if value < 2.5 then 1 else if value < 3.5 then 0 else -1
  | "Interest Cover Ratio" ->
      if value > 6.0 then 1 else if value > 4.0 then 0 else -1
  | "Debt-to-Equity Ratio" ->
      if value < 0.8 then 1 else if value < 1.5 then 0 else -1
  | "Equity Multiplier" ->
      if value < 1.8 then 1 else if value < 2.5 then 0 else -1
  | "Return on Assets (ROA)" ->
      if value > 12.0 then 1 else if value > 6.0 then 0 else -1
  | "Return on Equity (ROE)" ->
      if value > 18.0 then 1 else if value > 12.0 then 0 else -1
  | "Gross Profit Margin" ->
      if value > 45.0 then 1 else if value > 35.0 then 0 else -1
  | "Operating Margin" ->
      if value > 18.0 then 1 else if value > 12.0 then 0 else -1
  | "EBITDA Margin" ->
      if value > 25.0 then 1 else if value > 18.0 then 0 else -1
  | "Pre-Tax Margin" ->
      if value > 15.0 then 1 else if value > 10.0 then 0 else -1
  | _ -> 0

(* For testing purposes *)
let extract_prices_with_date json start_date end_date =
  let time_series =
    json
    |> Yojson.Basic.Util.member "Time Series (Daily)"
    |> Yojson.Basic.Util.to_assoc
  in
  List.fold_left
    (fun acc (date, data) ->
      if date >= start_date && date <= end_date then
        let close_price =
          data
          |> Yojson.Basic.Util.member "4. close"
          |> Yojson.Basic.Util.to_string |> float_of_string
        in
        (date, close_price) :: acc
      else acc)
    [] time_series

let extract_prices json =
  let time_series =
    json
    |> Yojson.Basic.Util.member "Time Series (Daily)"
    |> Yojson.Basic.Util.to_assoc
  in
  List.fold_left
    (fun acc (date, data) ->
      let close_price =
        data
        |> Yojson.Basic.Util.member "4. close"
        |> Yojson.Basic.Util.to_string |> float_of_string
      in
      (date, close_price) :: acc)
    [] time_series

let calculate_moving_average prices n =
  let rec take n lst =
    match lst with
    | x :: xs when n > 0 -> x :: take (n - 1) xs
    | _ -> []
  in
  let reversed_prices = List.rev prices in
  let p = if List.length prices > n then take n reversed_prices else prices in
  List.fold_left (fun acc (_, price) -> acc +. price) 0. p
  /. float_of_int (List.length p)

let rank_stock stock =
  let process_stock_data symbol statement =
    fetch_stock_data symbol statement >>= fun json ->
    let extracted_data = extract_data json in
    Lwt.return extracted_data
  in
  let fetch_ts_data symbol =
    fetch_stock_data symbol "TIME_SERIES_DAILY" >>= fun json ->
    let prices = extract_prices json in
    Lwt.return prices
  in
  process_stock_data stock "BALANCE_SHEET" >>= fun balance ->
  process_stock_data stock "INCOME_STATEMENT" >>= fun income ->
  fetch_ts_data stock >>= fun prices ->
  let ratios =
    List.concat
      [
        liquidity balance;
        efficiency balance income;
        leverage balance income;
        profitability balance income;
      ]
  in
  let sma_50 = calculate_moving_average prices 50 in
  let sma_200 = calculate_moving_average prices 200 in
  let moving_average_score =
    if sma_50 > sma_200 then 3 else if sma_50 < sma_200 then -3 else 0
  in
  let unscaled_score =
    List.fold_left
      (fun acc (name, value) -> acc + score_ratio name value)
      0 ratios
    + moving_average_score
  in
  let scaled_score =
    if List.length ratios = 0 then 0.0
    else float_of_int unscaled_score /. float_of_int (List.length ratios + 3)
  in
  Lwt.return scaled_score
