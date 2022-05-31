(*
   
type db0 = Set(Term.t hash_consed) String.Map.t

type pat_term =
  Lit
  App pat_term hash_consed

type env = Term.t hash_consed array hash_consed

(* Continuation. *)
type cont = env * pat_term

Ugh. This is 

type db_node = {
  parents : db_node list
  children : db_node list
  assumptions : db0;
  inferences : db0; (* *)
  triggers : clause (* rule to put something into parent. *)

} 

Normal form - binary

cont | head :- cont * pat
pat =  =>  | lit |   , ?

(* sorted array *)
Scope = Term.t hash_consed array hash_consed
Set String.Map.t Scope.Map.t

{ db : Set(Term)  ; rules :  ; triggers :    }  String.Map.t Scope.Map.t

ctx0 = [||]
ctx -> rel_label -> 


Probably hold off on harrop. It seems complicated and there might be a way to do it with first class lambdas


*)
