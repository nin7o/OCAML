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

let deuxieme_bis l = 
  if ((l != []) && (tl(l) != []))
  then hd(tl(l))
  else failwith "La liste contient moins de deux éléments"

(*
  n_a_zero : construit une liste décroissante de n à 0
  int -> int list
  il faut n >= 0 *)

let rec n_a_zero n =
  if (n < 0)
  then []
  else n::n_a_zero(n-1)



let zero_a_n n =
  let rec aux p =
    if p >= n
    then [n] 
    else p::aux(p + 1)
  in 
  if (n < 0)
  then failwith "n négatif"
  else aux 0
  (* Fonction non terminale *)

let zero_a_n_term n = 
  let rec aux p l =
    if (p < 0)
    then l 
    else aux (p - 1) (p::l)
  in 
  aux n []

  (* positions : donne les indices d'un élément dans une liste
    paramètres : un élément ('a) + une liste d'éléments ('a list)
    résultat : une liste qui contient les positions (int list)
    'a -> 'a list -> int list *)

let positions e l =
  let rec aux_pos l indice = 
    match l with
    | [] -> []
    | tete::reste -> 
        if tete = e
        then indice::(aux_pos reste (indice + 1))
        else (aux_pos reste (indice + 1))
  in aux_pos l 0;; 

  (*Exercice 2*)

(*
map : ('a list) -> ('a -> 'b) -> ('b list)
liste initiale -> fonction à appliquer -> liste resultat

fold : ('a list) -> ('a -> 'b -> 'a) -> 'b -> 'b
liste initiale -> fonction à appliquer -> élément neutre -> résultat
*)

let oppose l = map (fun x -> (-x)) l ;;

(*let somme l = fold (fun e r -> (e+r)) l 0;; woula la prof elle ecrit nimp *)

let map f l = fold_right(fun e r -> (f e)::r) l [] ;;
