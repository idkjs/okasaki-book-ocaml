module Make:
  (
    Approx: Finite_map_intf.S,
    Exact: Finite_map_intf.S,
    HashF: {let hash: Exact.key => Approx.key;},
  ) =>
   Finite_map_intf.S with type key = Exact.key;
