(* Evaluation des expressions simples *)


(* Module abstrayant les expressions *)
module type ExprSimple =
sig
  type t
  val const : int -> t
  val plus : t -> t -> t
  val mult : t-> t -> t
end

module type ExprVar =
sig
  type t
  type tname = string
  val var : tname -> t
  val def : (tname*t) -> t -> t  
end

module type Expr =
sig
  include ExprSimple
  include (ExprVar with type t := t)
end

(* Module réalisant l'évaluation d'une expression *)
module EvalSimple : ExprSimple with type t = int =
struct
  type t = int
  let const c = c
  let plus e1 e2 = e1 + e2
  let mult e1 e2 = e1 * e2
end

module PrintSimple : ExprSimple with type t = string =
struct 
  type t = string
  let const c = string_of_int c
  let plus e1 e2 = "("^e1^"+"^e2^")"
  let mult e1 e2 = "("^e1^"*"^e2^")"
end

module CompteSimple : ExprSimple with type t = int =
struct 
  type t = int 
  let const _c = 0
  let plus e1 e2 = e1 + e2 + 1 
  let mult e1 e2 = e1 + e2 + 1 
end

module PrintVar : ExprVar with type t = string = 
struct
  type t = string
  type tname = string
  let var n = n 
  let def (n,i) e = "let "^n^" = "^i^" in "^e
end

(* Solution 1 pour tester *)
(* A l'aide de foncteur *)

(* Définition des expressions *)
module ExemplesSimples (E:ExprSimple) =
struct
  (* 1+(2*3) *)
  let exemple1  = E.(plus (const 1) (mult (const 2) (const 3)) )
  (* (5+2)*(2*3) *)
  let exemple2 =  E.(mult (plus (const 5) (const 2)) (mult (const 2) (const 3)) )
end

(* Module d'évaluation des exemples *)
module EvalExemples =  ExemplesSimples (EvalSimple)

let%test _ = (EvalExemples.exemple1 = 7)
let%test _ = (EvalExemples.exemple2 = 42)

module PrintExemples = ExemplesSimples(PrintSimple)

let%test _ = (PrintExemples.exemple1 = "(1+(2*3))")

module CompteExemples = ExemplesSimples (CompteSimple)

let%test _ = (CompteExemples.exemple1 = 2)
let%test _ = (CompteExemples.exemple2 = 3)

module ExempleVar (E : Expr) = 
struct
  include ExemplesSimples (E)
  let exemple3 = E.def ("x", E.plus(E.const 1) (E.const 2)) (E.mult(E.var "x") (E.const 3))
end
