open Base;

type cell('a) =
  | Nil
  | Cons('a, t('a))
and t('a) = lazy_t(cell('a));

let rec (++) = (t1, t2) =>
  lazy(
    switch (t1) {
    | lazy Nil => Lazy.force(t2)
    | lazy ([@implicit_arity] Cons(h1, s1)) =>
      [@implicit_arity] Cons(h1, s1 ++ t2)
    }
  );

let rec take = (t, ~len) =>
  lazy(
    switch (t, len) {
    | (_, 0) => Nil
    | (lazy Nil, _) => Nil
    | (lazy ([@implicit_arity] Cons(h, s)), _) =>
      [@implicit_arity] Cons(h, take(s, ~len=len - 1))
    }
  );

/* let rec drop t ~len = lazy (
 *   match t, len with
 *   | _, 0 -> Lazy.force t
 *   | lazy Nil, _ -> Nil
 *   | lazy (Cons (_, s)), _ -> Lazy.force (drop s ~len:(len - 1))) */

let drop = (t, ~len) => {
  let rec aux = (t, n) =>
    switch (t, n) {
    | (_, 0) => t
    | (lazy Nil, _) => lazy(Nil)
    | (lazy ([@implicit_arity] Cons(_, s)), _) => aux(s, n - 1)
    };

  aux(t, len);
};

let rev = t => {
  let rec aux = acc =>
    fun
    | lazy Nil => acc
    | lazy ([@implicit_arity] Cons(h, s)) =>
      aux(lazy([@implicit_arity] Cons(h, acc)), s);

  aux(lazy(Nil), t);
};

let to_list = t => {
  let rec aux = acc =>
    fun
    | lazy Nil => acc
    | lazy ([@implicit_arity] Cons(h, s)) => aux([h, ...acc], s);

  aux([], t) |> List.rev;
};

let rec of_list = l =>
  lazy(
    switch (l) {
    | [] => Nil
    | [x, ...xs] => [@implicit_arity] Cons(x, of_list(xs))
    }
  );
