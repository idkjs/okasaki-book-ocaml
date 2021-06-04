module Make = (M: Finite_map_intf.S) => {
  type key = list(M.key);

  type t('a) =
    | Node(option('a), M.t(t('a)));

  let empty = [@implicit_arity] Node(None, M.empty);

  let rec lookup = (t, ~key) =>
    switch (key, t) {
    | ([], [@implicit_arity] Node(None, _)) => raise(Not_found)
    | ([], [@implicit_arity] Node(Some(x), _)) => x
    | ([x, ...xs], [@implicit_arity] Node(_, m)) =>
      let t' = M.lookup(m, ~key=x);
      lookup(t', ~key=xs);
    };

  let rec bind = (t, ~key, ~value) =>
    switch (key, t) {
    | ([], [@implicit_arity] Node(_, m)) =>
      [@implicit_arity] Node(Some(value), m)
    | ([x, ...xs], [@implicit_arity] Node(v, m)) =>
      let t' =
        try(M.lookup(m, ~key=x)) {
        | Not_found => empty
        };
      [@implicit_arity]
      Node(v, M.bind(m, ~key=x, ~value=bind(t', ~key=xs, ~value)));
    };
};
