(** [serve_static_file filename] reads the static file specified by [filename]
    from the "static" directory. 
    Returns: the content of the file as a string wrapped in an Lwt promise. *)
val serve_static_file : string -> string Lwt.t

(** [categorize_stock point_vals] generates a categorization string for each
    stock in [point_vals]. Each stock is categorized based on its rank.
    Requires: [point_vals] is a list of tuples where each tuple contains a stock
    symbol and its rank. *)
val categorize_stock : (string * float) list -> string

(** [find_optimal_destination rank positives] finds the optimal destination
    stock for reallocation based on the given [rank] and the list of
    positive-ranked stocks [positives]. 
    Requires: [positives] is a non-empty list of tuples, where each tuple 
    contains a stock symbol and its rank.
    Raises: Failure if [positives] is empty. *)
val find_optimal_destination : float -> ('a * float) list -> 'a

(** [generate_recommendations point_vals portfolio] generates a list of
    recommendations for reallocating assets from negatively ranked stocks to
    positively ranked stocks or other investments. 
    Requires: [point_vals] is a list of tuples containing stock symbols and 
    their ranks. [portfolio] is a valid portfolio containing stock symbols and 
    their allocations. 
    Returns: a list of tuples, where each tuple consists of a stock symbol, the amount to
    reallocate, and an optional destination stock. *)
val generate_recommendations :
  (string * float) list -> Portfolio.t -> (string * float * string option) list

(** [serve_input_page _req] serves the HTML content of the input page. 
    Returns: an HTTP response wrapped in an Lwt promise. *)
val serve_input_page : 'a -> Opium.Response.t Lwt.t

(** [serve_result_page point_vals portfolio] serves the result page with
    categorizations and recommendations. 
    Requires: [point_vals] is a list of tuples containing stock symbols and 
    their ranks. [portfolio] is a valid portfolio of stock allocations. 
    Returns: an HTTP response with the generated result page, wrapped in an 
    Lwt promise. *)
val serve_result_page :
  (string * float) list -> Portfolio.t -> Opium.Response.t Lwt.t

(** [process_form req] processes the form submission from the input page,
    extracts the portfolio data, and serves the result page with
    recommendations. 
    Requires: [req] is a valid HTTP request containing form data with stock 
    symbols and amounts. 
    Returns: an HTTP response with the result page, wrapped in an Lwt promise. *)
val process_form : Opium.Request.t -> Opium.Response.t Lwt.t
