open Okasaki_book;

module Make = (Key: Ordered_intf.S) => {
  type key = Key.t;

  type t('a) = list((key, 'a));

  let empty = [];

  let rec lookup = (t, ~key) =>
    switch (t) {
    | [] => raise(Not_found)
    | [(k, v), ...xs] =>
      if (Key.compare(key, k) == 0) {
        v;
      } else {
        lookup(xs, ~key);
      }
    };

  let bind = (t, ~key, ~value) => [(key, value), ...t];
};
