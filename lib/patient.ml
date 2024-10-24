type t = {
  name : string;
  diagnosis : string;
}

let create_patient name diagnosis = { name; diagnosis }

let priority (pat : t) =
  if pat.diagnosis = "Appendicitis" then 1
  else if pat.diagnosis = "Sprain" then 2
  else 3

let print_patient pat = pat.name ^ ", " ^ pat.diagnosis