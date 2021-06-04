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

  let weight =
    fun
    | E => 0
    | [@implicit_arity] T(w, _, _, _) => w;

  /* let make_t w v l r =
   *   if weight l <= weight r then
   *     T (w, v, r, l)
   *   else
   *     T (w, v, l, r)
   *
   * let rec merge t1 t2 =
   *   match t1, t2 with
   *   | E, _ -> t2
   *   | _, E -> t1
   *   | T (w1, v1, l1, r1), T (w2, v2, l2, r2) ->
   *     if Elm.(v1 <= v2) then
   *       make_t (w1 + w2) v1 l1 (merge r1 t2)
   *     else
   *       make_t (w1 + w2) v2 l2 (merge r2 t1) */

  let rec merge = (t1, t2) =>
    switch (t1, t2) {
    | (E, _) => t2
    | (_, E) => t1
    | (
        [@implicit_arity] T(w1, v1, l1, r1),
        [@implicit_arity] T(w2, v2, l2, r2),
      ) =>
      if (Elm.(v1 <= v2)) {
        if (weight(l1) <= w2 + weight(r1)) {
          [@implicit_arity] T(w1 + w2, v1, merge(r1, t2), l1);
        } else {
          [@implicit_arity] T(w1 + w2, v1, l1, merge(r1, t2));
        };
      } else if (weight(l2) <= w1 + weight(r2)) {
        [@implicit_arity] T(w1 + w2, v2, merge(t1, r2), l2);
      } else {
        [@implicit_arity] T(w1 + w2, v2, l2, merge(t1, r2));
      }
    };

  let insert = (t, ~value) => merge(t, [@implicit_arity] T(1, value, E, E));

  let find_min =
    fun
    | E => raise(Empty)
    | [@implicit_arity] T(_, v, _, _) => v;

  let delete_min =
    fun
    | E => raise(Empty)
    | [@implicit_arity] T(_, _, r, l) => merge(l, r);
};
