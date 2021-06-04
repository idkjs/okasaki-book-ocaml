open Core_bench;
open Okasaki_book;
open Base;

module US = Unbalanced_set.Make(Int);
module RBS = Red_black_set.Make(Int);
module RBS_2 = Red_black_set_2.Make(Int);

/* let nums = List.init 1_000_000 ~f:(fun _ -> Random.int Int.max_value) */
let nums = List.init(10_000, ~f=n => n) |> List.rev;

let () = {
  let f = (module S: Set_intf.S with type elm = Int.t, l: list(int)) => {
    let h =
      l |> List.fold(~init=S.empty, ~f=(h, n) => S.insert(h, ~value=n));
    l |> List.iter(~f=n => ignore @@ S.member(h, ~value=n));
  };

  [
    /* Bench.Test.create ~name:"Unbalanced set" (fun () -> f (module US) nums); */
    Bench.Test.create(~name="Red black set", () => f((module RBS), nums)),
    Bench.Test.create(~name="Red black set 2", () => f((module RBS_2), nums)),
  ]
  |> Bench.bench(
       ~display_config=Bench.Display_config.create(~show_percentage=true, ()),
     );
};
