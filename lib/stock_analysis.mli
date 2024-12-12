(** [score_ratio name value] evaluates the given financial ratio [name] with its
    corresponding [value] and assigns a score based on predefined thresholds.
    Requires: [name] is a valid ratio name. 
    Returns: an integer score where 1 indicates a good value, 0 indicates a neutral 
    value, and -1 indicates a poor value. *)
val score_ratio : string -> float -> int

(** [extract_prices_with_date json start_date end_date] extracts the closing
    prices of a stock between [start_date] and [end_date] from the given [json]
    data. 
    Requires: [json] contains valid stock data in the "Time Series (Daily)" 
    format. 
    Returns: a list of tuples, where each tuple contains a date and the 
    corresponding closing price. *)
val extract_prices_with_date :
  Yojson.Basic.t -> string -> string -> (string * float) list

(** [extract_prices json] extracts all available closing prices of a stock from
    the given [json] data. 
    Requires: [json] contains valid stock data in the "Time Series (Daily)" format. 
    Returns: a list of tuples, where each tuple contains a date and the 
    corresponding closing price. *)
val extract_prices : Yojson.Basic.t -> (string * float) list


(** [calculate_moving_average prices n] calculates the moving average of the
    last [n] closing prices from [prices]. 
    Requires: [prices] is a list of tuples containing dates and prices. [n] 
    is a positive integer. 
    Returns: the calculated moving average as a float. *)
val calculate_moving_average : ('a * float) list -> int -> float

(** [rank_stock stock] calculates the overall rank of the given stock symbol
    [stock] based on its financial ratios and moving average scores. 
    Requires: [stock] is a valid stock symbol and the necessary financial data can be
    fetched. 
    Returns: a scaled ranking score as a float, wrapped in an Lwt promise. *)
val rank_stock : string -> float Lwt.t

