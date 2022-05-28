open Js_of_ocaml

let _ =
  Js.export "Hashlog"
    (object%js
       (* what. run_string somehow is not acceptable? Underscores are not acceptable? *)
       method runhashlog x = Hashlog.run_string (Js.to_string x) |> Js.string
     end)