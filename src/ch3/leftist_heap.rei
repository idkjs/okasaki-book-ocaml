module Make:
  (Element: Ordered_intf.S) =>
   {
    include Heap_intf.S with module Elm = Element;
    let of_list: list(Elm.t) => t;
  };
