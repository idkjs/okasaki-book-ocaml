open Base;
open Stdio;
open Okasaki_book;

module H = Leftist_heap.Make(Int);

let%expect_test "test" = {
  let h =
    H.empty
    |> H.insert(~value=5)
    |> H.insert(~value=2)
    |> H.insert(~value=4)
    |> H.insert(~value=3);

  h |> H.find_min |> printf("%d\n");
  H.delete_min(h) |> H.find_min |> printf("%d\n");
  H.delete_min(H.delete_min(h)) |> H.find_min |> printf("%d\n");
  H.delete_min(H.delete_min(H.delete_min(h)))
  |> H.find_min
  |> printf("%d\n");

  %expect
  {|
    2
    3
    4
    5 |};
};

let%expect_test "test of_list" = {
  let h = H.of_list([5, 2, 4, 3]);

  h |> H.find_min |> printf("%d\n");
  H.delete_min(h) |> H.find_min |> printf("%d\n");
  H.delete_min(H.delete_min(h)) |> H.find_min |> printf("%d\n");
  H.delete_min(H.delete_min(H.delete_min(h)))
  |> H.find_min
  |> printf("%d\n");

  %expect
  {|
    2
    3
    4
    5 |};
};

let%expect_test "test of_list with empty list" = {
  let h = H.of_list([]);

  h |> H.is_empty |> printf("%b\n");

  %expect
  {| true |};
};

let%expect_test "test of_list with singleton" = {
  let h = H.of_list([5]);

  h |> H.find_min |> printf("%d\n");

  %expect
  {| 5 |};

  h |> H.delete_min |> H.is_empty |> printf("%b\n");

  %expect
  {| true |};
};
