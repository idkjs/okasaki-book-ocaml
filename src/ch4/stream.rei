type cell('a) =
  | Nil
  | Cons('a, t('a))
and t('a) = lazy_t(cell('a));

let (++): (t('a), t('a)) => t('a);

let take: (t('a), ~len: int) => t('a);

let drop: (t('a), ~len: int) => t('a);

let rev: t('a) => t('a);

let to_list: t('a) => list('a);

let of_list: list('a) => t('a);
