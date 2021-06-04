open Base;
open Stdio;
open Okasaki_book;

module T = Trie.Make((Assoc_list.Make(Char)));

let%expect_test "test" = {
  let l = [
    (String.to_list("cat"), 1),
    (String.to_list("car"), 2),
    (String.to_list("cart"), 3),
    (String.to_list("dog"), 4),
  ];
  let t =
    List.fold(l, ~init=T.empty, ~f=(acc, (key, value)) =>
      T.bind(acc, ~key, ~value)
    );

  List.iter(l, ~f=((key, _)) => T.lookup(t, ~key) |> printf("%d "));

  %expect
  {| 1 2 3 4 |};
};
