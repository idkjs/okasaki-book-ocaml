[@deriving sexp_of]
type t('a);

let empty: t('a);

let is_empty: t('a) => bool;

let cons: ('a, t('a)) => t('a);

let head: t('a) => 'a;

let (++): (t('a), t('a)) => t('a);

let update: (t('a), int, 'a) => t('a);

let suffixes: t('a) => t(t('a));
