/* Exercise 3.10 */
module Make = (Element: Ordered_intf.S) => {
  type elm = Element.t;

  type color =
    | R
    | B;

  type t =
    | E
    | T(color, elm, t, t);

  let empty = E;

  let rec member = (t, ~value) =>
    switch (t) {
    | E => false
    | [@implicit_arity] T(_, x, l, r) =>
      switch (Element.compare(value, x)) {
      | 0 => true
      | (-1) => member(l, ~value)
      | 1 => member(r, ~value)
      | _ => assert(false)
      }
    };

  let lbalance =
    fun
    | (
        B,
        z,
        [@implicit_arity] T(R, y, [@implicit_arity] T(R, x, a, b), c),
        d,
      )
    | (
        B,
        z,
        [@implicit_arity] T(R, x, a, [@implicit_arity] T(R, y, b, c)),
        d,
      ) =>
      [@implicit_arity]
      T(
        R,
        y,
        [@implicit_arity] T(B, x, a, b),
        [@implicit_arity] T(B, z, c, d),
      )
    | (c, v, l, r) => [@implicit_arity] T(c, v, l, r);

  let rbalance =
    fun
    | (
        B,
        x,
        a,
        [@implicit_arity] T(R, y, [@implicit_arity] T(R, z, b, c), d),
      )
    | (
        B,
        x,
        a,
        [@implicit_arity] T(R, y, b, [@implicit_arity] T(R, z, c, d)),
      ) =>
      [@implicit_arity]
      T(
        R,
        y,
        [@implicit_arity] T(B, x, a, b),
        [@implicit_arity] T(B, z, c, d),
      )
    | (c, v, l, r) => [@implicit_arity] T(c, v, l, r);

  let insert = (t, ~value) => {
    let rec ins =
      fun
      | E => [@implicit_arity] T(R, value, E, E)
      | [@implicit_arity] T(c, x, l, r) =>
        switch (Element.compare(value, x)) {
        | 0 => [@implicit_arity] T(c, x, l, r)
        | (-1) => lbalance((c, x, ins(l), r))
        | 1 => rbalance((c, x, l, ins(r)))
        | _ => assert(false)
        };

    switch (ins(t)) {
    | E => assert(false)
    | [@implicit_arity] T(_, x, l, r) => [@implicit_arity] T(B, x, l, r)
    };
  };
};
