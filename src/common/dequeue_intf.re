module type S = {
  exception Empty;

  type t('a);

  let empty: t('a);
  let is_empty: t('a) => bool;

  let cons: (t('a), ~value: 'a) => t('a);
  let head: t('a) => 'a;
  let tail: t('a) => t('a);

  let snoc: (t('a), ~value: 'a) => t('a);
  let last: t('a) => 'a;
  let init: t('a) => t('a);
};