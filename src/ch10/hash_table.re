module Make =
       (
         Approx: Finite_map_intf.S,
         Exact: Finite_map_intf.S,
         HashF: {let hash: Exact.key => Approx.key;},
       ) => {
  open HashF;

  type key = Exact.key;

  type t('a) = Approx.t(Exact.t('a));

  let empty = Approx.empty;

  let lookup = (t, ~key) =>
    Approx.lookup(t, ~key=hash(key)) |> Exact.lookup(~key);

  let bind = (t, ~key, ~value) => {
    let h = hash(key);
    let exact =
      try(Approx.lookup(t, ~key=h)) {
      | Not_found => Exact.empty
      };
    Approx.bind(t, ~key=h, ~value=Exact.bind(exact, ~key, ~value));
  };
};
