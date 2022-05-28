open Core
open Hashlog

let strip_pos (file : AST.file) =
  List.map file ~f:(fun (head, clauses) ->
      ( head,
        List.map clauses ~f:(fun lit ->
            match lit with
            | AST.LitPos p -> p
            | _ -> failwith "not a positive literal") ))

let main filename =
  (* print_endline filename; *)
  In_channel.with_file filename ~f:(fun chan ->
      let lexbuf = Lexing.from_channel chan in
      let file = Parser.parse_file Lexer.token lexbuf in
      let file = strip_pos file in
      run_loop file;
      let envs =
        search_pat String.Map.empty (AST.Apply ("output", [ AST.Var "o" ]))
      in
      List.iter envs ~f:(fun env ->
          let t = String.Map.find_exn env "o" in
          Format.printf "%a\n" Term.pp_term t))

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
