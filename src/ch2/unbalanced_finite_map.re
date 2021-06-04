module Make = (Key: Ordered_intf.S) => {
  type key = Key.t;
  type t('a) =
    | E
    | T(t('a), key, 'a, t('a));

  let empty = E;

  let rec bind = (t, ~key, ~value) =>
    switch (t) {
    | E => [@implicit_arity] T(E, key, value, E)
    | [@implicit_arity] T(l, k, v, r) =>
      switch (Key.compare(key, k)) {
      | 0 => [@implicit_arity] T(l, k, value, r)
      | (-1) => [@implicit_arity] T(bind(l, ~key, ~value), k, v, r)
      | 1 => [@implicit_arity] T(l, k, v, bind(r, ~key, ~value))
      | _ => assert(false)
      }
    };

  let rec lookup = (t, ~key) =>
    switch (t) {
    | E => raise(Not_found)
    | [@implicit_arity] T(l, k, v, r) =>
      switch (Key.compare(key, k)) {
      | 0 => v
      | (-1) => lookup(l, ~key)
      | 1 => lookup(r, ~key)
      | _ => assert(false)
      }
    };
};
