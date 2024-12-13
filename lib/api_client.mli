val fetch_stock_data : string -> string -> Yojson.Basic.t Lwt.t
(** Gets the data for a specific stock. [symbol] represents the ticker of the
    stock. Must be of type string. [statement] represents the financial
    statement requiested. Must be of type string and either "BALANCE_SHEET",
    "INCOME_STATEMENT", or "CASH_FLOW".*)

val extract_data : Yojson.Basic.t -> (string * string) list
(** Extracts financial data from a json and returns an associated list. [json]
    represents the json file being extracted from. Must be of type
    Yojson.Basic.t.*)

val extract_data_ts : Yojson.Basic.t -> (string * float) list
(** Extracts time series data from a json. [json] represents the json file being
    extracted from. Must be of type Yojson.Basic.t.*)

val get_val : (string * string) list -> string -> string
(** Extracts the specified value of an item from a financial statement. [item]
    represents the value being extracted. Must be of type string. [report]
    represents the financial statement being extracted from. Must be of type
    (string * string) list. *)
