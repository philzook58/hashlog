open Js_of_ocaml

let _ =
  Js.export "Hashlog"
    (object%js
       method run_string x = Hashlog.run_string (Js.to_string x) |> Js.string
    end)
