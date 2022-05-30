(* https://chargueraud.org/research/2009/ln/main.pdf *)

open Core_kernel

type ln =
  | FVar of string
  | FVarI of int
  | BVar of int
  | App of ln * ln
  | Lam of scope
[@@deriving equal, compare, sexp, hash]

and scope = { scope : ln } [@@deriving equal, compare, sexp, hash]

let print_ln t = Sexp.pp_hum Format.std_formatter (sexp_of_ln t)

type bf = Bound of int | Free of int

type ln_pat =
  | PFVar of string
  | PFVarI of int
  | PBVar of int
  | PApp of ln_pat * ln_pat
  | PLam of scope_pat
  | PVar of string * bf list

and scope_pat = { scope : ln_pat }

let rec varopen k e (t : ln) =
  match t with
  | FVar _ -> t
  | FVarI _ -> t
  | BVar i -> if Int.(i = k) then e else t
  | App (f, y) -> App (varopen k e f, varopen k e y)
  | Lam body -> Lam { scope = varopen (k + 1) e body.scope }

let instantiate (e : ln) (t : scope) : ln = varopen 0 e t.scope

let rec pvaropen k e (t : ln_pat) =
  match t with
  | PFVar _ -> t
  | PFVarI _ -> t
  | PBVar i -> if Int.(i = k) then e else t
  | PApp (f, y) -> PApp (pvaropen k e f, pvaropen k e y)
  | PLam body -> PLam { scope = pvaropen (k + 1) e body.scope }
  | PVar (p, args) ->
      PVar
        ( p,
          List.map
            ~f:(fun a ->
              match a with
              | Bound i ->
                  if Int.(i = k) then
                    match e with
                    | PFVarI j -> Free j
                    | _ ->
                        failwith
                          "tried to instantiate PVar with something not \
                           variable."
                  else a
              | Free _ -> a)
            args )

let pinstantiate (e : ln_pat) (t : scope_pat) : ln_pat = pvaropen 0 e t.scope

let rec varclose k x (t : ln) : ln =
  match t with
  | FVar x' -> if String.(x = x') then BVar k else t
  | FVarI _ -> t
  | BVar _ -> t
  | App (f, y) -> App (varclose k x f, varclose k x y)
  | Lam body -> Lam { scope = varclose (k + 1) x body.scope }

let rec varclose' k x (t : ln) : ln =
  match t with
  | FVarI x' -> if Int.(x = x') then BVar k else t
  | App (f, y) -> App (varclose' k x f, varclose' k x y)
  | Lam body -> Lam { scope = varclose' (k + 1) x body.scope }
  | _ -> t

let abstract (x : string) (t : ln) : scope = { scope = varclose 0 x t }

let fresh =
  let count = ref 0 in
  fun () ->
    let c = !count in
    count := c + 1;
    c

let abstract' (x : int) (t : ln) : scope = { scope = varclose' 0 x t }

let lam f =
  let x = fresh () in
  let t = abstract' x (f (FVarI x)) in
  Lam t

(*
let rec norm (t : ln) : ln =
  match t with
  | App (f,x) -> let f = norm f in
                 let x = norm x in
                 match f with
                 | Lam b -> norm (instantiate b x)
                 | _ -> App (f,x)
  | Lam b -> Lam (norm b)
  | _ -> t
*)

let rec norm (t : ln) : ln =
  match t with
  | Lam t' ->
      let v = fresh () in
      let t'' = instantiate (FVarI v) t' in
      Lam (abstract' v (norm t''))
  | App (t, u) -> (
      let t' = norm t in
      let u' = norm u in
      match t' with Lam t'' -> norm (instantiate u' t'') | _ -> App (t', u'))
  | _ -> t

let%expect_test _ =
  let id = lam (fun x -> x) in
  print_ln (norm (App (id, App (id, id))));
  [%expect {| (Lam ((scope (BVar 0)))) |}]

let%expect_test _ =
  let t = lam (fun x -> lam (fun _y -> x)) in
  let f = lam (fun _x -> lam (fun y -> y)) in
  print_ln (norm (App (App (t, f), t)));
  [%expect {| (Lam ((scope (Lam ((scope (BVar 0))))))) |}]

let multiapp (e : ln) (args : ln list) =
  List.fold args ~init:e ~f:(fun f x -> App (f, x))

(* Abstract out the FVarI (turn into bvar) *)
let multiabstract (t : ln) (args : bf list) =
  List.fold ~init:t args ~f:(fun t i -> 
    match i with
    | Free i -> Lam (abstract' i t)
    | Bound _ -> failwith "unexpect bound var in pattern")

let check_open_bvars (t : ln) =
  let rec worker k t =
    match t with
    | BVar i -> i >= k
    | FVarI _ -> false
    | FVar _ -> true
    | App (f, x) -> worker k f && worker k x
    | Lam b -> worker (k + 1) b.scope
  in
  worker 0 t

let rec millermatch env ln ln_pat =
  let ( let* ) x f = Option.bind x ~f in
  match (ln, ln_pat) with
  | FVar s, PFVar s' -> if String.(s = s') then Some env else None
  | FVarI s, PFVarI s' -> if Int.(s = s') then Some env else None
  | BVar _, _ -> failwith "shouldn't hit a bound var"
  | Lam b, PLam b' ->
      let v = fresh () in
      let b = instantiate (FVarI v) b in
      let b' = pinstantiate (PFVarI v) b' in
      millermatch env b b'
  | App (f, x), PApp (f', x') ->
      let* env = millermatch env f f' in
      millermatch env x x'
  | _, PVar (p, args) -> (
      (* args are de bruijn indices *)
      match String.Map.find env p with
      | None ->
          let ln = multiabstract ln args in
          if check_open_bvars ln then
            Some (String.Map.add_exn env ~key:p ~data:ln)
          else None
      | Some f ->
          let f = norm (multiapp f args) in
          if equal_ln ln f then Some env else None)
  | _, _ -> None
(*

sigma = signature?
Go just de bruin?

eta-long
beta normal -> no app(lam , )

There is probably an algorithmic way to take a disallowed shape and rearrange your type to disallow it
Nonempty list. disallow [] -> Nonempty of 'a * 'a list

eta long no app(app(pvar, ), )

full eta long depends on parameter being fully applied. Hmm. Which depends on the type?

type concterm = BVar
FVar
Lam of pattermterm
App of concterm * term

type patterm = PApply of string * pattermterm list | T of concterm

Miller patterns apply higher order variables only to terms they can't possible contain because of scoping issues.
Patterns are implicilty bound outside the scope of the lambdas

App (PVar v, x) ->

let rec millermatch badsig env ln ln_pat =
  | Lam b, Lam b' -> let x = fresh
       badsig = x :: badsig

I don't really see a reason to use locally nameless rather than pure de bruijn

let rec millermatch ln ln_pat = 
  let (let* ) f x = Option.bind x ~f in
    match ln, ln_pat with
    | FVar s, FVar s' -> String.(s = s') then Some env else None
    | BVar i, _ -> failwith "shouldn't hit a bound var"
    | Lam b, Lam b' ->   
      alphamatch env b b' in
    | App (f, x), App (f', x') ->
        let* env = Option.bind (alphamatch env f f') in
        alphamatch env x x'
    | _ , PVar (p, args) ->  (* args are de bruijn indices *)
       match String.find env p with
       | None -> 
        let ln' = abstract args ln in
        if no_unbound_bvar ln' then Some (String.Map.add env p ln') else None
       | Some e' -> 
          let e' = norm (App (e', args)) in
          if (ln = e') then Some env else None

Thesis used @ as application
F(a,b,c,d)

type patterm = 
  | Const of string * term list
  | App of term * term
  | Lam of term
  | Pat of string * int list (* unique integers *)
  | Var of int
By their nature now external constants are fully applied
if you want to talk about unapplied or partial application you need to put lambda binders on
  i.e. eta normal form.

In some sense the "app" in Pat and Const are totally different from App, but also similar.

let rec alphamatch ln ln_pat = 
  let (let* ) f x = Option.bind x ~f in
    match ln, ln_pat with
    | FVar s, FVar s' -> String.(s = s') then Some env else None
    | BVar i, _ -> failwith "shouldn't hit a bound var"
    | Lam b, Lam b' -> 
      let v = FVar (fresh ()) in  
      let* env = alphamatch env (instantiate v b) (instantiate v b') in
      String.Map.map env ~f:(fun t -> abstract v t) (* No. it should just plain fail*)
    | App (f, x), App (f', x') ->
        let* env = Option.bind (alphamatch env f f') in
        alphamatch env x x'
    | _ , PVar p -> 
       match String.find env p with
       | None -> Some (String.Map.add env p ln)
       | Some e' -> if (ln = e') then Some env else None

       *)
(*
Abstractions is picking all possible ways to abstract out subterm e
It is a relative of varclose
 
*)
(*
let abstractions k e t =
  if Ln.(e = t) then
    [e; BVar k]
  else
    match t with
    | Lam b -> List.map (abstractions (k+1) e b) ~f:(fun c -> Lam c)
    | App (f,x) -> List.bind (abstractions k e f) ~f:(fun f'
       List.map (abstractions k e x) ~f:(fun x' ->
          App (f', x') 
        )
    )
    | _ -> [t]

(*
Non determinism isn't that surprising, given that AC pattern matching has it.   
*)

(* instnating pattern such that when you normalize it it can become the lambda *)
let rec pmatch ln ln_pat = 
  match ln, ln_pat with
  | FVar s, FVar s' -> String.(s = s') then Some env else None
  | BVar i, _ -> failwith "shouldn't hit a bound var"
  | Lam b, Lam b' ->
  | 
  | _ , App (f, x) ->
  | _ , App (PVar f, x) ->
      if has env f then 
      else abstractions
      use_env 
      norm
      pattern_match
  | _ , App (Lam b, x) ->
          pmatch ln (instantiate x b)


 (\y y)  (\x  (F x))
 x (F q)
 \z
 t (F q) -> (\x. t[q -> x]  ) (all different ways of taking q to x)   
 





(*
 type expr = Var of string
  | App of expr * expr
  | Lam of string * expr
type postree = PTHere
 | PTLeftOnly of postree
 | PTRightOnly of postree
 | PTBoth of postree * postree

type structure = SVar 
 | SLam of postree option * structure
 | SApp of structure * structure

type varmap = postree String.Map.t
type esummary = {structure :  ; varmap : }

*)
*)
