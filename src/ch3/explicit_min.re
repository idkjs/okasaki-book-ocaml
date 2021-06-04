module Make = (Heap: Heap_intf.S) => {
  exception Empty;

  module Elm = Heap.Elm;

  type t =
    | E
    | NE(Elm.t, Heap.t);

  let empty = E;

  let is_empty =
    fun
    | E => true
    | NE(_) => false;

  let insert = (t, ~value) =>
    switch (t) {
    | E => [@implicit_arity] NE(value, Heap.insert(Heap.empty, ~value))
    | [@implicit_arity] NE(me, h) =>
      if (Elm.(value < me)) {
        [@implicit_arity] NE(value, Heap.insert(h, ~value));
      } else {
        [@implicit_arity] NE(me, Heap.insert(h, ~value));
      }
    };

  let merge = (t1, t2) =>
    switch (t1, t2) {
    | (E, _) => t2
    | (_, E) => t1
    | ([@implicit_arity] NE(me1, h1), [@implicit_arity] NE(me2, h2)) =>
      if (Elm.(me1 < me2)) {
        [@implicit_arity] NE(me1, Heap.merge(h1, h2));
      } else {
        [@implicit_arity] NE(me2, Heap.merge(h1, h2));
      }
    };

  let find_min =
    fun
    | E => raise(Empty)
    | [@implicit_arity] NE(me, _) => me;

  let delete_min =
    fun
    | E => raise(Empty)
    | [@implicit_arity] NE(_, h) => {
        let h' = Heap.delete_min(h);
        if (Heap.is_empty(h')) {
          E;
        } else {
          [@implicit_arity] NE(Heap.find_min(h'), h');
        };
      };
};
