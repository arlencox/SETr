#ifdef PKG_CUDD
module Cudd = SETr_Symbolic_BDD_Cudd
#endif
#ifdef PKG_MLBDD
module MLBDD = SETr_Symbolic_BDD_MLBDD
#endif
module XBDD = SETr_Symbolic_BDD_XBDD

#ifdef PKG_CUDD
module Default = Cudd.Make(struct let reorder = None end)
module Opt = SETr_Symbolic_Packer.Make(SETr_Symbolic_Remap.Make(Default))
#else
#ifdef PKG_MLBDD
module Default = MLBDD
module Opt = SETr_Symbolic_Equality.Make(SETr_Symbolic_Packer.Make(Default))
#else
module Default = XBDD
module Opt = SETr_Symbolic_Equality.Make(SETr_Symbolic_Packer.Make(Default))
#endif
#endif


