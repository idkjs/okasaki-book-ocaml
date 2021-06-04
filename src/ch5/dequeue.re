open Base;

exception Empty;

type t('a) = (list('a), list('a));

let empty = ([], []);

let is_empty =
  fun
  | ([], []) => true
  | ([_, ..._], _)
  | (_, [_, ..._]) => false;

let split = l => {
  let rec loop = (n, l, acc) =>
    switch (n, l) {
    | (0, _)
    | (_, []) => (List.rev(acc), List.rev(l))
    | (n, [x, ...xs]) => loop(n - 1, xs, [x, ...acc])
    };

  loop(List.length(l) / 2, l, []);
};

let check = t =>
  switch (t) {
  | ([], [_])
  | ([_], []) => t
  | ([], r) =>
    let (a, b) = split(r);
    (b, a);
  | (f, []) =>
    let (a, b) = split(f);
    (a, b);
  | ([_, ..._], [_, ..._]) => t
  };

let cons = (t, ~value) =>
  switch (t) {
  | ([x], []) => ([value], [x])
  | (f, r) => ([value, ...f], r)
  };

let head =
  fun
  | ([], [x]) => x
  | ([], _) => raise(Empty)
  | ([x, ..._], _) => x;

let tail =
  fun
  | ([], [_]) => empty
  | ([], _) => raise(Empty)
  | ([_, ...f], r) => check((f, r));

let snoc = (t, ~value) =>
  switch (t) {
  | ([], [x]) => ([x], [value])
  | (f, r) => (f, [value, ...r])
  };

let last =
  fun
  | ([x], []) => x
  | (_, []) => raise(Empty)
  | (_, [x, ..._]) => x;

let init =
  fun
  | ([_], []) => empty
  | (_, []) => raise(Empty)
  | (f, [_, ...r]) => check((f, r));
