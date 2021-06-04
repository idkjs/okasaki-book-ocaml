module Make = (Element: Ordered_intf.S) => {
  type elm = Element.t;
  type t =
    | E
    | T(t, elm, t);

  let empty = E;

  let rec insert = (t, ~value as x) =>
    switch (t) {
    | E => [@implicit_arity] T(E, x, E)
    | [@implicit_arity] T(l, y, r) =>
      switch (Element.compare(x, y)) {
      | 0 => [@implicit_arity] T(l, x, r)
      | (-1) => [@implicit_arity] T(insert(l, ~value=x), y, r)
      | 1 => [@implicit_arity] T(l, y, insert(r, ~value=x))
      | _ => assert(false)
      }
    };

  let rec member = (t, ~value as x) =>
    switch (t) {
    | E => false
    | [@implicit_arity] T(l, y, r) =>
      switch (Element.compare(x, y)) {
      | 0 => true
      | (-1) => member(l, ~value=x)
      | 1 => member(r, ~value=x)
      | _ => false
      }
    };

  let rec complete = (~value as x, ~depth as d) =>
    switch (d) {
    | 0 => empty
    | d when d < 0 => raise(Invalid_argument("negative argument"))
    | d =>
      let subtree = complete(~value=x, ~depth=d - 1);
      [@implicit_arity] T(subtree, x, subtree);
    };
};
