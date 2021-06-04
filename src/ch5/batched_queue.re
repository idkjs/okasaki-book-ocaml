exception Empty;

type t('a) = (list('a), list('a));

let empty = ([], []);

let is_empty =
  fun
  | ([], []) => true
  | ([_, ..._], _)
  | (_, [_, ..._]) => false;

let head =
  fun
  | ([], _) => raise(Empty)
  | ([h, ..._], _) => h;

let check_f =
  fun
  | ([], r) => (List.rev(r), [])
  | t => t;

let tail =
  fun
  | ([], _) => raise(Empty)
  | ([_, ...f], r) => check_f((f, r));

let snoc = ((f, r), ~value) => check_f((f, [value, ...r]));
