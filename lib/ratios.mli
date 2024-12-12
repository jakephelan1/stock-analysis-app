(** Retrieves associated values. [key] represents the name of the item being inspected. Must be 
   of type string. [report] represents the list that holds all the information, specifically the one key is seeking.
   Must be of type (string*string) list. *)
val safe_float_of_get_val : (string * string) list -> string -> float

(** Produces the cash ratio for a company. [balance] represents the balance sheet
    of the company being evaluated. Must be of type (string * string) list.
    Over 0.5 is a good ratio. Below 0.5 is a bad ratio. *)
val cash_ratio : (string * string) list -> float

(** Produces the acid test ratio for a company. [balance] represents the balance sheet
    of the company being evaluated. Must be of type (string * string) list.
    Over 1.0 is a good ratio. Below 1.0 is a bad ratio. *)
val acid_test_ratio : (string * string) list -> float

(** Produces the current ratio for a company. [balance] represents the balance sheet
    of the company being evaluated. Must be of type (string * string) list.
    Over 1.5 is a good ratio. Below 1.5 is a bad ratio. *)
val current_ratio : (string * string) list -> float

(** Produces a list of liquidity ratios for a company. [balance] represents the balance sheet
    of the company being evaluated. Must be of type (string * string) list. *)
val liquidity : (string * string) list -> (string * float) list

(** Produces the days receivables for a company. [balance] represents the balance sheet
    of the company being evaluated. [income] represents the income statement of the company.
    Must be of type (string * string) list. Below 45 is a good value. Above 45 is a bad value. *)
val days_receivables : (string * string) list -> (string * string) list -> float

(** Produces the days payable outstanding for a company. [balance] represents the balance sheet
    of the company being evaluated. [income] represents the income statement of the company.
    Must be of type (string * string) list. Below 60 is a good value. Above 60 is a bad value. *)
val days_payable_outstanding : (string * string) list -> (string * string) list -> float

(** Produces the days of inventory for a company. [balance] represents the balance sheet
    of the company being evaluated. [income] represents the income statement of the company.
    Must be of type (string * string) list. Below 90 is a good value. Above 90 is a bad value. *)
val days_of_inventory : (string * string) list -> (string * string) list -> float

(** Produces a list of efficiency ratios for a company. [balance] represents the balance sheet,
    and [income] represents the income statement. Both must be of type (string * string) list. *)
val efficiency : (string * string) list -> (string * string) list -> (string * float) list

(** Produces the debt to assets ratio for a company. [balance] represents the balance sheet
    of the company being evaluated. Must be of type (string * string) list.
    Below 0.6 (60%) is a good ratio. Above 0.6 (60%) is a bad ratio. *)
val debt_to_assets_ratio : (string * string) list -> float

(** Produces the total debt to EBITDA ratio for a company. [balance] represents the balance sheet
    of the company being evaluated. [income] represents the income statement of the company.
    Must be of type (string * string) list. Below 4.0 is a good ratio. Above 4.0 is a bad ratio. *)
val total_debt_to_ebitda : (string * string) list -> (string * string) list -> float

(** Produces the interest cover ratio for a company. [income] represents the income statement
    of the company being evaluated. Must be of type (string * string) list.
    Above 1.5 is a good ratio. Below 1.5 is a bad ratio. *)
val interest_cover_ratio : (string * string) list -> float

(** Produces the debt to equity ratio for a company. [balance] represents the balance sheet
    of the company being evaluated. Must be of type (string * string) list.
    Below 1.5 (150%) is a good ratio. Above 1.5 (150%) is a bad ratio. *)
val debt_to_equity_ratio : (string * string) list -> float

(** Produces the equity multiplier ratio for a company. [balance] represents the balance sheet
    of the company being evaluated. Must be of type (string * string) list.
    Below 3.0 is a good ratio. Above 3.0 is a bad ratio. *)
val equity_multiplier : (string * string) list -> float

(** Produces a list of leverage ratios for a company. [balance] represents the balance sheet,
    and [income] represents the income statement. Both must be of type (string * string) list. *)
val leverage : (string * string) list -> (string * string) list -> (string * float) list

(** Produces the return on assets value for a company. [balance] represents the balance sheet
    of the company being evaluated. [income] represents the income statement of the company.
    Must be of type (string * string) list. Above 5% is a good value. Below 5% is a bad value. *)
val return_on_assets : (string * string) list -> (string * string) list -> float

(** Produces the return on equity value for a company. [balance] represents the balance sheet
    of the company being evaluated. [income] represents the income statement of the company.
    Must be of type (string * string) list. Above 10% is a good value. Below 10% is a bad value. *)
val return_on_equity : (string * string) list -> (string * string) list -> float

(** Produces the gross profit margin value for a company. [income] represents the income statement
    of the company being evaluated. Must be of type (string * string) list.
    Above 20% is a good value. Below 20% is a bad value. *)
val gross_profit_margin : (string * string) list -> float

(** Produces the operating margin value for a company. [income] represents the income statement
    of the company being evaluated. Must be of type (string * string) list.
    Above 10% is a good value. Below 10% is a bad value. *)
val operating_margin : (string * string) list -> float

(** Produces the EBITDA margin value for a company. [income] represents the income statement
    of the company being evaluated. Must be of type (string * string) list.
    Above 15% is a good value. Below 15% is a bad value. *)
val ebitda_margin : (string * string) list -> float

(** Produces the pre-tax margin value for a company. [income] represents the income statement
    of the company being evaluated. Must be of type (string * string) list.
    Above 10% is a good value. Below 10% is a bad value. *)
val pre_tax_margin : (string * string) list -> float

(** Produces a list of profitability ratios for a company. [balance] represents the balance sheet,
    and [income] represents the income statement. Both must be of type (string * string) list. *)
val profitability : (string * string) list -> (string * string) list -> (string * float) list