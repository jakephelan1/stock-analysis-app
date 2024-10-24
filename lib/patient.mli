type t

val priority : t -> int
(** Decides the level of priority patients should be placed at (sorted by
    sickness). [pat] Represents the patient. Must be of type t.*)

val create_patient : string -> string -> t
(** Create a patient object. [Name] Represents the name of the patient. Must be
   of type string. [Diagnosis Represents the sickness of the patient. Must be of
   type string.*)

val print_patient : t -> string
(** Prints the name and diagnosis of a patient object.. [pat] Represents the
    patient. Must be of type t.*)
