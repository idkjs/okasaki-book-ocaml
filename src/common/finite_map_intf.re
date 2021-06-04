module type S = {
  type key;
  type t('a);

  let empty: t('a);
  let bind: (t('a), ~key: key, ~value: 'a) => t('a);
  let lookup: (t('a), ~key: key) => 'a;
};
