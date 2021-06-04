module Make:
  (Element: Ordered_intf.S) =>
   {
    include Set_intf.S with type elm = Element.t;
    let complete: (~value: elm, ~depth: int) => t;
  };
