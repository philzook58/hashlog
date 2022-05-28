open Core_kernel
module AST = AST
module Parser = Parser
module Lexer = Lexer

module Term = struct
  type t = Apply of { head : string; args : t list } | Int of int
  [@@deriving equal, compare, hash, sexp]

  let rec pp_term ppf term =
    match term with
    | Apply { head = f; args = [] } -> Format.fprintf ppf "%s" f
    | Apply { head = f; args } ->
        Format.fprintf ppf "%s(%a)" f (AST.pp_list_comma pp_term) args
    | Int i -> Format.fprintf ppf "%d" i
end
(*
Some redundant info that might be nice.
f, nargs -> []
maintain parents
Can 

*)

type hash_consed = { node : Term.t; tag : int; hkey : int }

(* Var | Lam | App *)
let table = Hashtbl.create (module Term)

let apply head args =
  let x : Term.t = Apply { head; args } in
  Hashtbl.find_or_add table x ~default:(fun () -> x)

let myint i =
  let x : Term.t = Int i in
  Hashtbl.find_or_add table x ~default:(fun () -> x)
(*
The tablified version:???
maybe keyed on number of args?


Oh. 
AST.term is already patterm. That makes sense.
*)
(*
type patterm = 
 | Fun of string * patterm list 
 | Var of string
 *)

type env = Term.t String.Map.t

let rec pmatch (env : env) (p : AST.term) (t : Term.t) : env option =
  match (p, t) with
  | Apply (f, args'), Apply { head; args } ->
      if String.(f = head) && Int.equal (List.length args) (List.length args')
      then
        List.fold2_exn args' args ~init:(Some env) ~f:(fun acc arg' arg ->
            Option.bind acc ~f:(fun env -> pmatch env arg' arg))
      else None
  | Int i, Int j -> if Int.(i = j) then Some env else None
  | Int _, Apply _ -> None
  | Apply _, Int _ -> None
  | Var p, _ -> (
      match String.Map.find env p with
      | None -> Some (String.Map.add_exn env ~key:p ~data:t)
      | Some t' -> if Poly.(t = t') then Some env else None)

let search_pat env (p : AST.term) =
  let ts = Hashtbl.keys table in
  List.filter_map ts ~f:(pmatch env p)

let search_multi_pat (body : AST.term list) =
  List.fold body ~init:[ String.Map.empty ] ~f:(fun acc p ->
      List.bind acc ~f:(fun env -> search_pat env p))

let instan (env : env) (head : AST.term) : Term.t =
  let rec worker p =
    match p with
    | AST.Apply (f, args) -> apply f (List.map ~f:worker args)
    | Int i -> myint i
    | Var p -> String.Map.find_exn env p
  in
  worker head

type clause = AST.term * AST.term list

let run_clause (clause : clause) =
  let head, body = clause in
  let envs = search_multi_pat body in
  List.iter envs ~f:(fun env ->
      let (_ : Term.t) = instan env head in
      ())

let run_loop (prog : clause list) =
  let rec worker n =
    List.iter prog ~f:run_clause;
    let n' = Hashtbl.length table in
    if Int.(n = n') then () else worker n'
  in
  worker (Hashtbl.length table)

(*
Doing it as an embedded DSL.   
*)
let%expect_test _ =
  let edge i j =
    let (_ : Term.t) = apply "edge" [ myint i; myint j ] in
    ()
  in
  edge 1 2;
  edge 2 3;
  edge 3 4;
  let edge i j = AST.Apply ("edge", [ i; j ]) in
  let path i j = AST.Apply ("path", [ i; j ]) in
  let prog =
    let open AST in
    let i, j, k = (Var "i", Var "j", Var "k") in
    [ (path i j, [ edge i j ]); (path i k, [ edge i j; path j k ]) ]
  in
  run_loop prog;
  let () =
    let ts = Hashtbl.data table in
    Format.pp_print_list
      ~pp_sep:(fun pp () -> Format.pp_print_string pp ", ")
      Term.pp_term Format.std_formatter ts;
    print_endline ""
  in
  ();
  [%expect {| path(3,4), path(1,3), 1, path(2,3), edge(2,3), 4, 2, path(2,4), path(1,4), edge(1,2), edge(3,4), path(1,2), 3 |}]
