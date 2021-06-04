open Base;

module Make = (Element: Ordered_intf.S) => {
  exception Empty;

  module Elm = Element;

  type t =
    | E
    | T(int, Elm.t, t, t);

  let empty = E;

  let is_empty =
    fun
    | E => true
    | T(_) => false;

  let rank =
    fun
    | E => 0
    | [@implicit_arity] T(k, _, _, _) => k;

  let make_t = ((v, l, r)) =>
    if (rank(l) >= rank(r)) {
      [@implicit_arity] T(rank(r) + 1, v, l, r);
    } else {
      [@implicit_arity] T(rank(l) + 1, v, r, l);
    };

  let rec merge = (t1, t2) =>
    switch (t1, t2) {
    | (E, t2) => t2
    | (t1, E) => t1
    | (
        [@implicit_arity] T(_, v1, l1, r1),
        [@implicit_arity] T(_, v2, l2, r2),
      ) =>
      if (Elm.(v1 <= v2)) {
        make_t((v1, l1, merge(r1, t2)));
      } else {
        make_t((v2, l2, merge(t1, r2)));
      }
    };

  /* let insert t ~value = merge t (T (0, value, E, E)) */

  let rec insert = (t, ~value) =>
    switch (t) {
    | E => [@implicit_arity] T(1, value, E, E)
    | [@implicit_arity] T(_, v, l, r) =>
      if (Elm.(value <= v)) {
        [@implicit_arity] T(1, value, t, E);
      } else {
        make_t((v, l, insert(r, ~value)));
      }
    };

  let find_min =
    fun
    | E => raise(Empty)
    | [@implicit_arity] T(_, v, _, _) => v;

  let delete_min =
    fun
    | E => raise(Empty)
    | [@implicit_arity] T(_, _, l, r) => merge(l, r);

  let of_list = l => {
    let elm_to_node = v => [@implicit_arity] T(1, v, E, E);
    let rec merge_nodes = (ns: list(t), acc: list(t)): t =>
      switch (ns, acc) {
      | ([], []) => E
      | ([], [x]) => x
      | ([], acc) => merge_nodes(acc, [])
      | ([x], acc) => merge_nodes([], [x, ...acc])
      | ([x, y, ...xs], acc) => merge_nodes(xs, [merge(x, y), ...acc])
      };

    l |> List.map(~f=elm_to_node) |> (ns => merge_nodes(ns, []));
  };
};
