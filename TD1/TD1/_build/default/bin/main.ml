open Hashtbl;;
open List;;
open Inline_test_config;;

(* hd : 'a list -> 'a || tête de la liste
   tl : 'a list -> 'a list || liste sans la têtOpen_text
   @ : 'a list -> 'a list -> 'a list || fusion de deux listes*)

(* Exercice 1*)

(* deuxième fonction qui retourne le second élément d'une liste
  'a list -> a'
  exception à lever : la liste doit contenir au moins deux éléments *)

let deuxieme l =
  match l with
  | _::e2::_ -> e2
  | _ -> failwith "La liste contient moins de deux éléments"

let%test _ = deuxieme([1;2;3]) = 2

let deuxieme_bis l = 
  if ((l != []) && (tl(l) != []))
  then hd(tl(l))
  else failwith "La liste contient moins de deux éléments"

let%test _ = deuxieme_bis([4;5;6]) = 5

(*
  n_a_zero : construit une liste décroissante de n à 0
  int -> int list
  il faut n >= 0 *)

let rec n_a_zero n =
  if (n < 0)
  then []
  else n::n_a_zero(n-1)

