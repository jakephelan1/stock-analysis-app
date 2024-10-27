open Csv

let read_data file =
  let data = Csv.load file in
  let map = List.map (fun x -> x) data in
  List.concat map

let rec max acc lst =
  match lst with
  | [] -> acc
  | h :: t ->
      if int_of_string h > acc then max (int_of_string h) t else max acc t

let rec min acc lst =
  match lst with
  | [] -> acc
  | h :: t ->
      if int_of_string h < acc then min (int_of_string h) t else max acc t
