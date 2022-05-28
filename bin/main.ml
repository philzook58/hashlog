open Core
open Hashlog



let main filename =
  (* print_endline filename; *)
  In_channel.with_file filename ~f:(fun chan ->
      let lexbuf = Lexing.from_channel chan in
      let file = Parser.parse_file Lexer.token lexbuf in
      let output = run_file file in
      print_endline output)


let regular_file =
  Command.Arg_type.create (fun filename ->
      match Sys.is_file filename with
      | `Yes -> filename
      | `No -> failwith "Not a regular file"
      | `Unknown -> failwith "Could not determine if this was a regular file")

let command =
  Command.basic ~summary:"Hashlog baby!"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command filename = anon ("filename" %: regular_file) in
     fun () -> main filename)

let () = Command.run ~version:"1.0" command
