type t

val empty : t
(** [empty] is the empty portfolio. *)

val add_stock : string * float -> t -> t
(** [add_stock (stock, amount) portfolio] adds a stock [stock] with its
    associated [amount] to the [portfolio]. If [stock] already exists, it will
    appear twice in the portfolio, as duplicates are not merged. Requires:
    [amount] is non-negative. Returns: a new portfolio with the stock added. *)

val remove_stock : string -> t -> t
(** [remove_stock stock portfolio] removes all instances of [stock] from the
    [portfolio]. Returns: a new portfolio with [stock] removed. *)

val get_point_vals : t -> (string -> float Lwt.t) -> (string * float) list Lwt.t
(** [get_point_vals portfolio f] applies the function [f] to each stock in the
    [portfolio] to compute point values. Requires: [f] is a function that maps a
    stock symbol to a float wrapped in an Lwt promise. Returns: a list of tuples
    where each tuple contains a stock symbol and its computed point value,
    wrapped in an Lwt promise. *)

val to_list : t -> (string * float) list
(** [to_list portfolio] converts the [portfolio] to a list of stock symbols and
    their associated amounts. Returns: the underlying association list
    representation of the portfolio. *)

val rev : t -> t
(** [rev portfolio] reverses the order of stocks in the [portfolio]. Returns: a
    new portfolio with reversed order. *)

val mem : string -> t -> (string * float) option
(** [mem stock portfolio] checks if [stock] is in the [portfolio]. Returns:
    [Some (stock, amount)] if [stock] exists in the [portfolio], or [None] if it
    does not exist. *)
