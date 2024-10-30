open Lwt.Infix
open Opium
open Stock_logic

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
        fetch_stock_data symbol)
      (fun ex ->
        Lwt.return `Null)
  in

  if stock_data = `Null then
    serve_result_page
      "Failed to fetch stock data." 
  else
    let prices = extract_prices stock_data start_date end_date in
    if List.is_empty prices then serve_result_page "Invalid Start/End Dates"
    else
      let moving_avg = calculate_moving_average prices 7 in
      let result =
        Printf.sprintf "Stock Symbol: %s\n7-Day Moving Average: %.2f" symbol
          moving_avg
      in
      serve_result_page result