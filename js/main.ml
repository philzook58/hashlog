open Js_of_ocaml

let _ =
  Js.export "MyHashlog"
    (object%js
       (* what. run_string somehow is not acceptable? Underscores are not acceptable? *)
       method dohashlog x = Hashlog.run_string (Js.to_string x) |> Js.string
     end)