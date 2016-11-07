module Interface = SETr_Numeric_Value_Interface
module Interval = SETr_Numeric_Value_Interval
module Debug = SETr_Numeric_Value_Debug

let _ =
  let open SETr_DomainRegistrar in
  let build = function
    | [] ->
        NumericValue (module Interval)
    | _ -> build_error "Expected no arguments"
  in
  let args = "" in
  let help = "Builds an interval value domain" in
  register "numeric.value.interval" build args help
