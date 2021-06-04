module type S = {
  type elm;
  type t;

  let empty: t;
  let insert: (t, ~value: elm) => t;
  let member: (t, ~value: elm) => bool;
};
