open Base;

[@deriving sexp_of]
type t('a) =
  | Nil
  | Cons('a, t('a));

let empty = Nil;

let is_empty =
  fun
  | Nil => true
  | Cons(_) => false;

let cons = (h, t) => [@implicit_arity] Cons(h, t);

let head =
  fun
  | Nil => failwith("Stack is empty")
  | [@implicit_arity] Cons(x, _) => x;

let rec (++) = (t1, t2) =>
  switch (t1) {
  | Nil => t2
  | [@implicit_arity] Cons(x, xs) => [@implicit_arity] Cons(x, xs ++ t2)
  };

let rec update = (t, i, a) =>
  switch (t, i) {
  | (Nil, _) => failwith("subscript")
  | ([@implicit_arity] Cons(_, xs), 0) => [@implicit_arity] Cons(a, xs)
  | ([@implicit_arity] Cons(x, xs), i) =>
    [@implicit_arity] Cons(x, update(xs, i - 1, a))
  };

let rec suffixes =
  fun
  | Nil => Nil
  | [@implicit_arity] Cons(_, xs) as t =>
    [@implicit_arity] Cons(t, suffixes(xs));
