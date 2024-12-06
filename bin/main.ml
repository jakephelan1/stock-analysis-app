open Opium
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Lwt.Infix
open Final_Project.Web_server

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  App.empty
  |> App.get "/" serve_input_page
  |> App.post "/results" process_form
  |> App.run_command
