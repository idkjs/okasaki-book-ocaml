module Make:
  (M: Finite_map_intf.S) => Finite_map_intf.S with type key = list(M.key);
