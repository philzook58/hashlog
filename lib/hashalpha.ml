(* https://chargueraud.org/research/2009/ln/main.pdf *)
type ln = FVar of string | BVar of int | App of ln * ln | Lam of ln
type scope = ln

type ln_pat =
  | FVar of string
  | BVar of int
  | App of ln * ln
  | Lam of ln
  | PVar of string

  let rec varopen k e (t : ln) = match t with
  | FVar _ -> t
  | BVar i -> if Int.(i = k) then e else t
  | App (f,y) -> App (varopen k x f, varopen k x y)
  | Lam body -> Lam (varopen (k + 1) x body)
 
 let instantiate (e : ln) (t : scope) = varopen 0 e t 
 
 let rec varclose k x (t : ln) : ln = match t with
  | FVar x' -> if String.(x = x') then BVar k else t
  | BVar i -> t
  | App (f,y) -> App (varclose k x f, varclose k x y)
  | Lam body -> Lam (varclose (k + 1) x body)
 
 let abstract (x : string) t : ln =
   varclose 0 x t
(*
let rec alphamatch ln ln_pat = 
  let (let* ) f x = Option.bind x ~f in
    match ln, ln_pat with
    | FVar s, FVar s' -> String.(s = s') then Some env else None
    | BVar i, _ -> failwith "shouldn't hit a bound var"
    | Lam b, Lam b' -> 
      let v = FVar (fresh ()) in  
      let* env = alphamatch env (instantiate v b) (instantiate v b') in
      String.Map.map env ~f:(fun t -> abstract v t)
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
