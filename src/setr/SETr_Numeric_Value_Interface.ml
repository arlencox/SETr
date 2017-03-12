module L = SETr_Numeric_Logic

module type S = sig
  include SETr_Interface.Value
    with type cnst = int
     and type uop = L.uop
     and type bop = L.bop
     and type cop = L.cop
     and type output = int L.t
     and type sexp = int L.e
end
