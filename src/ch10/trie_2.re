module Make = (M: Finite_map_intf.S) => {
  type key = list(M.key);

  type t('a) =
    | Entry('a)
    | Node(M.t(t('a)));

  let empty = Node(M.empty);

  let rec lookup = (t, ~key) =>
    switch (key, t) {
    | ([], Node(_))
    | ([_, ..._], Entry(_)) => raise(Not_found)
    | ([], Entry(x)) => x
    | ([x, ...xs], Node(m)) =>
      let t' = M.lookup(m, ~key=x);
      lookup(t', ~key=xs);
    };

  let rec bind = (t, ~key, ~value) =>
    switch (key, t) {
    | ([], _) => Entry(value)
    | ([x, ...xs], Entry(_)) =>
      Node(M.bind(M.empty, ~key=x, ~value=bind(empty, ~key=xs, ~value)))
    | ([x, ...xs], Node(m)) =>
      let t' =
        try(M.lookup(m, ~key=x)) {
        | Not_found => empty
        };
      Node(M.bind(m, ~key=x, ~value=bind(t', ~key=xs, ~value)));
    };
};
