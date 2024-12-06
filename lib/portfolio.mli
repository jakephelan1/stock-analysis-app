type t

val empty : t
val add_stock : string * float -> t -> t
val remove_stock : string -> t -> t
val get_point_vals : t -> (string -> float Lwt.t) -> (string * float) list Lwt.t
val to_list : t -> (string * float) list
val rev : t -> t
val mem : string -> t -> (string * float) option
