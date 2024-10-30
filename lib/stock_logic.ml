(* lib/stock_logic.ml *)
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Lwt.Infix

let fetch_stock_data symbol =
  let api_key = "8XFVI3GT0MDOXWBN" in
  let uri =
    Uri.of_string
      (Printf.sprintf
         "https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=%s&apikey=%s"
         symbol api_key)
  in
  Cohttp_lwt_unix.Client.get uri >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body >|= fun response_body ->
  Yojson.Basic.from_string response_body

let extract_prices json start_date end_date =
  let time_series =
    json |> Yojson.Basic.Util.member "Time Series (Daily)" |> Yojson.Basic.Util.to_assoc
  in
  List.fold_left
    (fun acc (date, data) ->
      if date >= start_date && date <= end_date then
        let close_price =
          data |> Yojson.Basic.Util.member "4. close" |> Yojson.Basic.Util.to_string |> float_of_string
        in
        (date, close_price) :: acc
      else acc)
    [] time_series

let rec take n lst =
  match lst with 
  | [] -> []
  | x :: xs when n > 0 -> x :: take (n - 1) xs 
  | _ -> []

let calculate_moving_average prices n =
  let reversed_prices = List.rev prices in
  let p = if List.length prices > n then take n reversed_prices else prices in
  List.fold_left (fun acc (_, price) -> acc +. price) 0. p /. float_of_int (List.length p)
