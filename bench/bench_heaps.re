open Core_bench;
open Okasaki_book;
open Base;

module LH = Leftist_heap.Make(Int);
module WBLH = Weight_biased_leftist_heap.Make(Int);
module BH = Binomial_heap.Make(Int);

let () = {
  let nums = List.init(1_000_000, ~f=_ => Random.int(Int.max_value));
  let insert_and_delete =
      (module H: Heap_intf.S with type Elm.t = Int.t, l: list(int)) => {
    let rec del_loop = h =>
      if (H.is_empty(h)) {
        ();
      } else {
        del_loop(H.delete_min(h));
      };

    let h =
      l |> List.fold(~init=H.empty, ~f=(h, n) => H.insert(h, ~value=n));
    del_loop(h);
  };

  [
    Bench.Test.create(~name="Lefitst Heap", () =>
      insert_and_delete((module LH), nums)
    ),
    Bench.Test.create(~name="Weight Biased Lefitst Heap", () =>
      insert_and_delete((module WBLH), nums)
    ),
    Bench.Test.create(~name="Binomial Heap", () =>
      insert_and_delete((module BH), nums)
    ),
  ]
  |> Bench.bench(
       ~display_config=Bench.Display_config.create(~show_percentage=true, ()),
     );
};
