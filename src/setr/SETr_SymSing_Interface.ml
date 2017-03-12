module L = SETr_SymSing_Logic

module type S = sig
  include SETr_Interface.Domain
    with type sym = int
     and type cnstr = int L.t
     and type output = int L.t
     and type query = int L.q
end
