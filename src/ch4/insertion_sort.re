let rec f = (s: Stream.t('a)): Stream.t('a) => {
  open Stream;
  let rec insert = (x, s) =>
    lazy(
      switch (s) {
      | lazy Nil => [@implicit_arity] Cons(x, lazy(Nil))
      | lazy ([@implicit_arity] Cons(h, t)) =>
        if (x <= h) {
          [@implicit_arity] Cons(x, s);
        } else {
          [@implicit_arity] Cons(h, insert(x, t));
        }
      }
    );

  lazy(
    switch (s) {
    | lazy Nil => Nil
    | lazy ([@implicit_arity] Cons(h, s)) => Lazy.force @@ insert(h, f(s))
    }
  );
};
