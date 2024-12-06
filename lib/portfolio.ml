open Lwt.Infix

type t = (string * float) list

let empty : t = []
let add_stock (stock : string * float) (portfolio : t) : t = stock :: portfolio

let remove_stock (symbol : string) (portfolio : t) : t =
  List.filter (fun stock -> fst stock <> symbol) portfolio

let get_point_vals (portfolio : t) (f : string -> float Lwt.t) =
  Lwt_list.fold_left_s
    (fun acc stock ->
      let sym = fst stock in
      f sym >>= fun point_val -> Lwt.return ((sym, point_val) :: acc))
    [] portfolio

let mem (symbol : string) (pf : t) : (string * float) option =
  List.find_opt (fun (sym, _) -> sym = symbol) pf

let to_list (pf : t) : 'a list = pf
let rev (pf : t) : t = List.rev pf
