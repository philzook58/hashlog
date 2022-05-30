open Core_kernel
module AST = AST
module Parser = Parser
module Lexer = Lexer
open Hashcons

(*
   Just reuse hashcons infra?
*)
(*
module Symbol = struct
  type t = string hash_consed
  let symtable : (string, t) Hashtbl.t = Hashtbl.create (module String)
  let sym (s : string) : t = Hashtbl.find_or_add symtable s ~default:(fun () -> {sym = s})
  let equal (x : t) (y : t) = Poly.equal x y
  let (=) x y = equal x y
  let compare (x : t) (y : t) = Poly.compare x y

end *)
module Symbol = Hashcons.Make (String)

type symbol = string hash_consed

let symtable = Symbol.create 251
let sym s = Symbol.hashcons symtable s
let symbol_equal (x : symbol) (y : symbol) = Poly.equal x y

module Term = struct
  type t = Apply of symbol * hashcons_t array | Int of int
  and hashcons_t = t hash_consed

  let equal x y =
    match (x, y) with
    | Apply (f, x), Apply (f', x') ->
        symbol_equal f f' && Array.equal (fun x y -> Int.(x.tag = y.tag)) x x'
    | Int i, Int j -> Int.(i = j)
    | _, _ -> false

  let hash : t -> int = function
    | Int s -> Int.hash s
    | Apply (f, args) ->
        Array.fold args ~init:f.hkey ~f:(fun acc arg -> (arg.tag * 19) + acc)

  let rec pp_term ppf term =
    match term.node with
    | Apply (f, [||]) -> Format.fprintf ppf "%s" f.node
    | Apply (f, args) ->
        Format.fprintf ppf "%s(%a)" f.node
          (AST.pp_list_comma pp_term)
          (Array.to_list args)
    | Int i -> Format.fprintf ppf "%d" i
end
(*
Some redundant info that might be nice.
f, nargs -> []
maintain parents
Can 

*)

(* type hash_consed = { node : Term.t; tag : int; hkey : int } *)

(* Var | Lam | App *)
module Table = Hashcons.Make (Term)

let table = Table.create 251
let clear () = Table.clear table

(*
   It's nice to have table as implicit in everything
   type db = (Term.t, Term.t) Hashtbl.t
   let mk_db : db = Hashtbl.create (module Term) *)

let apply f args = Table.hashcons table (Apply (f, args))
let myint i = Table.hashcons table (Int i)
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

type env = Term.t hash_consed String.Map.t

(*
Intrinsic functions
"=" as a special case.   
*)
(*

   type db = (Term.t hash_consed) HMap(Symbol).t

   type patterm =
     | Lit of Term.t hash_consed
     | PApp of symbol * patterm array
     | PVar of string
   If I keep my database seperate from my hash cons
*)
let rec pmatch (env : env) (p : AST.term) (t : Term.t hash_consed) : env option
    =
  match (p, t.node) with
  | Apply (f, x), Apply (f', x') ->
      if symbol_equal (sym f) f' && Int.equal (List.length x) (Array.length x')
      then
        let x = Array.of_list x in
        Array.fold2_exn x' x ~init:(Some env) ~f:(fun acc arg' arg ->
            Option.bind acc ~f:(fun env -> pmatch env arg arg'))
      else None
  | Int i, Int j -> if Int.(i = j) then Some env else None
  | Int _, Apply _ -> None
  | Apply _, Int _ -> None
  | Var p, _ -> (
      match String.Map.find env p with
      | None -> Some (String.Map.add_exn env ~key:p ~data:t)
      | Some t' -> if Poly.(t = t') then Some env else None)

let iter_pat env (p : AST.term) f =
  Table.iter
    (fun t -> match pmatch env p t with None -> () | Some env -> f env)
    table

(*
let search_pat env (p : AST.term) =
  let ts = Hashtbl.keys table in
  List.filter_map ts ~f:(pmatch env p)

let search_multi_pat (body : AST.term list) =
  List.fold body ~init:[ String.Map.empty ] ~f:(fun acc p ->
      List.bind acc ~f:(fun env -> search_pat env p))
*)
let instan (env : env) (head : AST.term) : Term.t hash_consed =
  let rec worker p =
    match p with
    | AST.Apply (f, args) ->
        apply (sym f) (Array.of_list (List.map ~f:worker args))
    | Int i -> myint i
    | Var p -> String.Map.find_exn env p
  in
  worker head

type clause = AST.term * AST.term list

(*
let run_clause (clause : clause) =
  let head, body = clause in
  let envs = search_multi_pat body in
  List.iter envs ~f:(fun env ->
      let (_ : Term.t) = instan env head in
      ())
      *)
let run_clause (head, body) =
  let rec worker env = function
    | [] ->
        let (_ : Term.t hash_consed) = instan env head in
        ()
    | pat :: body ->
        Table.iter
          (fun t ->
            match pmatch env pat t with
            | None -> ()
            | Some env -> worker env body)
          table
  in
  worker String.Map.empty body

let run_loop (prog : clause list) =
  let rec worker n =
    List.iter prog ~f:run_clause;
    let _, n', _, _, _, _ = Table.stats table in
    if Int.(n = n') then () else worker n'
  in
  let _, n, _, _, _, _ = Table.stats table in
  worker n

let strip_pos (file : AST.file) =
  List.map file ~f:(fun (head, clauses) ->
      ( head,
        List.map clauses ~f:(fun lit ->
            match lit with
            | AST.LitPos p -> p
            | _ -> failwith "not a positive literal") ))

let run_file file =
  let file = strip_pos file in
  run_loop file;
  iter_pat String.Map.empty
    (AST.Apply ("output", [ AST.Var "o" ]))
    (fun env ->
      let t = String.Map.find_exn env "o" in
      Format.fprintf Format.str_formatter "%a\n" Term.pp_term t);
  Format.flush_str_formatter ()

let from_string (prog : string) =
  clear ();
  let lexbuf = Lexing.from_string prog in
  Parser.parse_file Lexer.token lexbuf

let run_string (prog : string) = run_file (from_string prog)

(*
Doing it as an embedded DSL.   
*)
let%expect_test _ =
  let edge i j =
    let (_ : Term.t hash_consed) = apply (sym "edge") [| myint i; myint j |] in
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
  let () = Table.iter (fun t -> Format.printf "%a ," Term.pp_term t) table in
  ();
  [%expect
    {| 3 ,edge(3,4) ,4 ,path(2,3) ,path(1,3) ,path(2,4) ,edge(1,2) ,path(1,4) ,1 ,edge(2,3) ,path(3,4) ,2 ,path(1,2) , |}]
