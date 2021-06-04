open Base;

module Make = (Element: Ordered_intf.S) => {
  exception Empty;

  module Elm = Element;

  type tree =
    | Node(int, Elm.t, list(tree));

  type t = list(tree);

  let empty = [];

  let is_empty =
    fun
    | [] => true
    | [_, ..._] => false;

  let link =
      (
        [@implicit_arity] Node(r, x1, c1) as tr1,
        [@implicit_arity] Node(_, x2, c2) as tr2,
      ) =>
    if (Elm.(x1 <= x2)) {
      [@implicit_arity] Node(r + 1, x1, [tr2, ...c1]);
    } else {
      [@implicit_arity] Node(r + 1, x2, [tr1, ...c2]);
    };

  let rank = ([@implicit_arity] Node(r, _, _)) => r;

  let root = ([@implicit_arity] Node(_, x, _)) => x;

  let rec insert_tree = (tr: tree): (t => t) =>
    fun
    | [] => [tr]
    | [tr', ...ts] as t =>
      switch (Int.compare(rank(tr), rank(tr'))) {
      | (-1) => [tr, ...t]
      | 0 => insert_tree(link(tr, tr'), ts)
      | _ => assert(false)
      };

  let insert = (t, ~value) =>
    insert_tree([@implicit_arity] Node(0, value, []), t);

  let rec merge = (t1, t2) =>
    switch (t1, t2) {
    | ([], _) => t2
    | (_, []) => t1
    | ([tr1, ...ts1], [tr2, ...ts2]) =>
      switch (Int.compare(rank(tr1), rank(tr2))) {
      | (-1) => [tr1, ...merge(ts1, t2)]
      | 1 => [tr2, ...merge(t1, ts2)]
      | 0 => insert_tree(link(tr1, tr2), merge(ts1, ts2))
      | _ => assert(false)
      }
    };

  let rec remove_min_tree = (t: t): (tree, t) =>
    switch (t) {
    | [] => raise(Empty)
    | [tr] => (tr, [])
    | [tr, ...ts] =>
      let (min_tr, remaining_t) = remove_min_tree(ts);
      if (Elm.(root(tr) < root(min_tr))) {
        (tr, ts);
      } else {
        (min_tr, [tr, ...remaining_t]);
      };
    };

  let find_min = t => {
    let (min_tr, _) = remove_min_tree(t);
    root(min_tr);
  };

  let delete_min = t => {
    let (min_tr, remaining_t) = remove_min_tree(t);
    let [@implicit_arity] Node(_, _, c) = min_tr;
    merge(List.rev(c), remaining_t);
  };
};
