open Opium
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Lwt.Infix

module StockData = struct
  type t = {
    symbol : string;
    prices : (string * float) list;
  }

  let yojson_of_t t =
    let prices_json =
      List.map
        (fun (date, price) ->
          `Assoc [ ("date", `String date); ("price", `Float price) ])
        t.prices
    in
    `Assoc [ ("symbol", `String t.symbol); ("prices", `List prices_json) ]

  let t_of_yojson yojson =
    match yojson with
    | `Assoc [ ("symbol", `String symbol); ("prices", `List prices) ] ->
        let prices =
          List.map
            (function
              | `Assoc [ ("date", `String date); ("price", `Float price) ] ->
                  (date, price)
              | _ -> failwith "Invalid price format")
            prices
        in
        { symbol; prices }
    | _ -> failwith "Invalid stock data JSON"
end

(* Helper function to take the first n elements from a list *)
let rec take n lst =
  match (lst, n) with
  | [], _ -> []
  | _, 0 -> []
  | x :: xs, n -> x :: take (n - 1) xs

let fetch_stock_data symbol =
  let api_key = "8XFVI3GT0MDOXWBN" in
  let uri =
    Uri.of_string
      (Printf.sprintf
         "https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=%s&apikey=%s"
         symbol api_key)
  in
  Logs.info (fun m -> m "Fetching stock data from API for symbol: %s" symbol);
  (* Debugging statement before API call *)
  Cohttp_lwt_unix.Client.get uri >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body >|= fun response_body ->
  Logs.info (fun m -> m "Received response from API for symbol: %s" symbol);
  (* Debugging statement after receiving response *)
  Yojson.Basic.from_string response_body

let serve_static_file filename =
  let file_path = Printf.sprintf "static/%s" filename in
  Lwt_io.with_file ~mode:Lwt_io.input file_path Lwt_io.read

let serve_result_page result =
  serve_static_file "result.html" >>= fun template ->
  let result_page =
    Str.global_replace (Str.regexp "{{result}}") result template
  in
  let headers = Opium.Headers.of_list [ ("Content-Type", "text/html") ] in
  let response =
    Opium.Response.make ~status:`OK ~headers
      ~body:(Opium.Body.of_string result_page)
      ()
  in
  Lwt.return response

let extract_prices json start_date end_date =
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

let calculate_moving_average prices n =
  let rec moving_avg_helper prices acc count =
    match prices with
    | [] -> List.rev acc
    | _ when count < n ->
        moving_avg_helper (List.tl prices) (List.hd prices :: acc) (count + 1)
    | _ ->
        let window = take n prices in
        let avg =
          List.fold_left (fun sum (_, price) -> sum +. price) 0. window
          /. float_of_int n
        in
        moving_avg_helper (List.tl prices)
          ((fst (List.hd prices), avg) :: acc)
          (count + 1)
  in
  moving_avg_helper prices [] 0

let serve_input_page _req =
  serve_static_file "index.html" >>= fun content ->
  let headers = Opium.Headers.of_list [ ("Content-Type", "text/html") ] in
  let response =
    Opium.Response.make ~status:`OK ~headers
      ~body:(Opium.Body.of_string content)
      ()
  in
  Lwt.return response

let process_form req =
  let%lwt body = Opium.Request.to_plain_text req in
  let params = Uri.query_of_encoded body in
  let find_param name =
    match List.assoc_opt name params with
    | Some values -> List.hd values
    | None ->
        Logs.err (fun m -> m "Missing parameter: %s" name);
        failwith (Printf.sprintf "Missing parameter: %s" name)
  in
  let symbol = find_param "symbol" in
  let start_date = find_param "start_date" in
  let end_date = find_param "end_date" in

  (* Fetch stock data and handle potential errors *)
  let%lwt stock_data =
    Lwt.catch
      (fun () ->
        Logs.info (fun m ->
            m "Attempting to fetch stock data for symbol: %s" symbol);
        (* Debugging statement before API call *)
        fetch_stock_data symbol)
      (fun ex ->
        Logs.err (fun m ->
            m "Error fetching stock data for symbol '%s': %s" symbol
              (Printexc.to_string ex));
        Lwt.return `Null)
  in

  if stock_data = `Null then
    serve_result_page
      "Failed to fetch stock data." (* Return a user-friendly error message *)
  else
    (* Continue processing if stock_data is valid *)
    let prices = extract_prices stock_data start_date end_date in
    if List.is_empty prices then serve_result_page "Invalid Start/End Dates"
    else
      let moving_avg = calculate_moving_average prices 7 in
      let result =
        Printf.sprintf "Stock Symbol: %s\n7-Day Moving Average: %.2f" symbol
          (snd (List.hd moving_avg))
      in
      serve_result_page result

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  App.empty
  |> App.get "/" serve_input_page
  |> App.post "/process" process_form
  |> App.run_command
