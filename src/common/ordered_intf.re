module type S = {
  type t;

  let compare: (t, t) => int;
  let (==): (t, t) => bool;
  let (<): (t, t) => bool;
  let (<=): (t, t) => bool;
};
