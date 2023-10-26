(*  Exercice à rendre **)
(*  PGCD -> pgcd.ml
 pgcd : int -> int -> int*
 Fonction qui calcule le plus grand commun diviseur entre deux entiers
 Paramètre a : premier entier du pgcd
 Paramètre b : second entier du pgcd
 Resultat : int, le PGCD entre les deux entiers du tuple true
 Precondition : a != 0 && b != 0*)

let pgcd (a : int) (b : int) =

  let rec aux a b =
    if a = b then a
    else
      if a > b then aux (a - b) (b)
      else aux (a) (b - a)
  in
  
  let abs a =
    if a < 0 then (-a)
    else a
  in 

    aux (abs a) (abs b)

(* Test unitaires *)
let%test _ = pgcd 3 5 = 1
let%test _ = pgcd 14 28 = 14
let%test _ = pgcd 56 98 = 14
let%test _ = pgcd 54 24 = 6
let%test _ = pgcd 17 23 = 1
(* Il faudrait aussi tester si une exception est levée
   dans le cas ou la précondition n'est pas respectée *)
