open Csv

let () =
  let data = Final_Project.Stocks.read_data "data/example_stock.csv" in

  List.iter (fun s -> print_endline s) data;

  let max_value = Final_Project.Stocks.max 0 data in
  Printf.printf "Max: %d\n" max_value;

  let min_value = Final_Project.Stocks.min 100 data in
  Printf.printf "Min: %d\n" min_value
