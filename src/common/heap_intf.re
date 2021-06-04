module type S = {
  exception Empty;

  module Elm: Ordered_intf.S;
  type t;

  let empty: t;
  let is_empty: t => bool;

  let insert: (t, ~value: Elm.t) => t;
  let merge: (t, t) => t;

  let find_min: t => Elm.t;
  let delete_min: t => t;
};
