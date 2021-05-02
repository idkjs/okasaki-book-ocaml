open Core_bench
open Okasaki_src
open Okasaki_common
open Base

let display_config = Bench.Display_config.create ~show_percentage:true ()

let gen_nums size = List.init size ~f:(fun _ -> Random.int Int.max_value)

let insert_and_delete (module H : Heap_intf.S with type Elm.t = Int.t) (l : int list) =
  let h = l |> List.fold ~init:H.empty ~f:(fun h n -> H.insert h ~value:n) in
  let rec del_loop h =
    if H.is_empty h then ()
    else del_loop (H.delete_min h)
  in
  del_loop h

module LH = Leftist_heap.Make(Int)
module WBLH = Weight_biased_leftist_heap.Make(Int)

let () =
  let nums = gen_nums 1_000_000 in
  [
    Bench.Test.create ~name:"Lefitst Heap" (fun () -> insert_and_delete (module LH) nums);
    Bench.Test.create ~name:"Weight Biased Lefitst Heap" (fun () -> insert_and_delete (module WBLH) nums);
  ]
  |> Bench.bench ~display_config
