open Core_bench;
open Okasaki_book;
open Base;

module BQ = Batched_queue;

let () = {
  let f = (module Q: Queue_intf.S) => {
    let rec loop = (n, q: Q.t(int)) =>
      if (n == 0) {
        ();
      } else {
        switch (Q.is_empty(q), Random.bool()) {
        | (true, _)
        | (false, true) => Q.snoc(q, ~value=1) |> loop(n - 1)
        | (false, false) => Q.tail(q) |> loop(n - 1)
        };
      };

    loop(10_000_000, Q.empty);
  };

  [Bench.Test.create(~name="Batched queue", () => f((module BQ)))]
  |> Bench.bench(
       ~display_config=Bench.Display_config.create(~show_percentage=true, ()),
     );
};
