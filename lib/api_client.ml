open Lwt.Infix
(* For statement, must enter BALANCE_SHEET, INCOME_STATEMENT, or CASH_FLOW *)

let fetch_stock_data symbol statement =
  let api_key = "FFNUH1UT0BB64ME8" in

  let uri =
    Uri.of_string
      (Printf.sprintf
         "https://www.alphavantage.co/query?function=%s&symbol=%s&apikey=%s"
         statement symbol api_key)
  in

  Cohttp_lwt_unix.Client.get uri >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body >|= fun response_body ->
  Yojson.Basic.from_string response_body

let extract_data json =
  let reports =
    json
    |> Yojson.Basic.Util.member "annualReports"
    |> Yojson.Basic.Util.to_list
  in
  List.hd reports |> Yojson.Basic.Util.to_assoc
  |> List.map (fun (key, value) ->
         ( key,
           Yojson.Basic.Util.to_string_option value
           |> Option.value ~default:(Yojson.Basic.to_string value) ))

let extract_data_ts json =
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

let get_val report item =
  try List.assoc item report
  with Not_found -> Printf.sprintf "Item %s not found in the report" item
