open Base;
open Stdio;
open Okasaki_book;

module Q = Batched_queue;

let%expect_test "test" = {
  let q = Q.(empty |> snoc(~value=1) |> snoc(~value=2) |> snoc(~value=3));

  q |> Q.head |> printf("%d ");
  q |> Q.tail |> Q.head |> printf("%d ");
  q |> Q.tail |> Q.tail |> Q.head |> printf("%d\n");

  %expect
  {| 1 2 3 |};
};
