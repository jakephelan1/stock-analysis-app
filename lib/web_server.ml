open Lwt.Infix
open Opium
open Portfolio
open Ratios
open Stock_analysis

let serve_static_file filename =
  let file_path = Printf.sprintf "static/%s" filename in
  Lwt_io.with_file ~mode:Lwt_io.input file_path Lwt_io.read

let categorize_stock point_vals =
  let categorize rank =
    if rank > 0.6 then "Very Good: " ^ string_of_float rank
    else if rank > 0.2 then "Good: " ^ string_of_float rank
    else if rank > -0.2 then "Neutral: " ^ string_of_float rank
    else if rank > -0.6 then "Poor: " ^ string_of_float rank
    else "Very Poor: " ^ string_of_float rank
  in
  let categorized =
    List.mapi
      (fun i (sym, ranking) ->
        sym ^ ": " ^ categorize ranking
        ^ if i < List.length point_vals - 1 then "," else "")
      point_vals
  in
  String.concat " " categorized

let find_optimal_destination rank positives =
  let get_max stocks =
    match stocks with
    | [] -> failwith "The list is empty; no rankings available."
    | (symbol, ranking) :: t ->
        List.fold_left
          (fun (max_symbol, max_ranking) (current_symbol, current_ranking) ->
            if current_ranking > max_ranking then
              (current_symbol, current_ranking)
            else (max_symbol, max_ranking))
          (symbol, ranking) t
        |> fst
  in
  match
    List.find_opt
      (fun (sym, rank2) -> abs_float ((rank *. -1.) -. rank2) < 0.3)
      positives
  with
  | Some (sym, _) -> sym
  | None -> get_max positives

let generate_recommendations point_vals portfolio =
  let positives = List.filter (fun stock -> snd stock > 0.) point_vals in
  let negatives = List.filter (fun stock -> snd stock < 0.) point_vals in
  if List.length negatives = 0 then []
  else
    List.map
      (fun (sym, ranking) ->
        let assets =
          match mem sym portfolio with
          | Some (_, a) -> ranking *. -1. *. a
          | None -> failwith "Expected stock not found in portfolio"
        in
        let dest =
          if List.length positives = 0 then None
          else Some (find_optimal_destination ranking positives)
        in
        (sym, assets, dest))
      negatives

let print_recommendations recs =
  if List.is_empty recs then
    "Keep your money where it currently is. Your stock picks seem to be \
     performing well"
  else
    List.fold_left
      (fun acc (sym, assets, dest) ->
        acc ^ "Move $" ^ string_of_int assets ^ " to " ^ dest ^ "\n")
      "" recs

let serve_input_page _req =
  serve_static_file "index.html" >>= fun content ->
  let headers = Opium.Headers.of_list [ ("Content-Type", "text/html") ] in
  let response =
    Opium.Response.make ~status:`OK ~headers
      ~body:(Opium.Body.of_string content)
      ()
  in
  Lwt.return response

let serve_result_page point_vals portfolio =
  serve_static_file "result.html" >>= fun template ->
  let categorizations = categorize_stock point_vals in
  let recommendations = generate_recommendations point_vals portfolio in

  let formatted_recommendations =
    List.map
      (fun (sym, assets, dest) ->
        Printf.sprintf "<li>Move $%.2f from %s to %s</li>" assets sym
          (match dest with
          | Some d -> d
          | None -> "No Destination"))
      recommendations
    |> String.concat "\n"
  in

  let result =
    Printf.sprintf
      "<h3>Categorizations:</h3><p>%s</p><h3>Recommendations:</h3><ul>%s</ul>"
      categorizations formatted_recommendations
  in

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

let process_form req =
  Opium.Request.to_plain_text req >>= fun body ->
  let params = Uri.query_of_encoded body in
  let rec build_portfolio acc idx =
    match
      ( List.assoc_opt (Printf.sprintf "symbol[%d]" idx) params,
        List.assoc_opt (Printf.sprintf "amount[%d]" idx) params )
    with
    | Some [ symbol ], Some [ amount ] ->
        build_portfolio
          (add_stock (symbol, float_of_string amount) acc)
          (idx + 1)
    | _ -> rev acc
  in
  let portfolio = build_portfolio empty 0 in
  get_point_vals portfolio rank_stock >>= fun point_vals ->
  serve_result_page point_vals portfolio
