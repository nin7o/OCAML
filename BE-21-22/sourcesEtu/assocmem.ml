open Util
open Mem

(* get_assoc: 'a -> 'a * 'b list -> 'b -> 'b
   Retourne la valeur associée à la clef e dans la liste l. si la clef n'est pas 
   présente dans la liste, renvoie def
   Paramètres :
    e : La clef 
    l : la liste associative
    def : la valeur a retourner par défaut
 *)
let rec get_assoc e l def = 
  match l with 
  | [] -> def
  | (a, b)::tl ->
      if (a = e) then
        b 
      else
        get_assoc e tl def

let%test _ = get_assoc 3 [(1,"a");(2,"b");(3,"c")] "d" = "c"
let%test _ = get_assoc 4 [(1,"a");(2,"b");(3,"c")] "d" = "d"
let%test _ = get_assoc 3 [] "d" = "d"

(* set_assoc : 'a -> 'a * 'b list -> 'b -> 'a * 'b list 
   Remplace la valeur associée à la clef e dans la liste l par x, ou ajoute le 
   couple (e,x) si la celf n'existe pas 
   Paramètres :
     e : la clef
     l : la liste associattive
     x : la valeur a ajouter
 *)
let rec set_assoc e l x = 
  match l with 
  | [] -> [(e, x)]
  | (a, b)::tl ->
      if a = e then 
        (a, x)::tl 
      else
        (a, b)::(set_assoc e tl x)


(* Tests unitaires : TODO *)
let%test _ = set_assoc 3 [(1,"a");(2,"b");(3,"c")] "d" = [(1,"a");(2,"b");(3,"d")]
let%test _ = set_assoc 4 [(1,"a");(2,"b");(3,"c")] "d" = [(1,"a");(2,"b");(3,"c");(4,"d")]
let%test _ = set_assoc 1 [(1,"a");(2,"b");(3,"c")] "b" = [(1,"b");(2,"b");(3,"c")]

module AssocMemory : Memory =
struct
    (* Type = liste qui associe des adresses (entiers) à des valeurs (caractères) *)
    type mem_type = (int * char) list

    (* Un type qui contient la mémoire + la taille de son bus d'adressage *)
    type mem = int * mem_type

    (* Nom de l'implémentation *)
    let name = "assoc"

    (* Taille du bus d'adressage *)
    let bussize (bs, _) = bs

    (* Taille maximale de la mémoire *)
    let size (bs, _) = pow2 bs

    (* Taille de la mémoire en mémoire *)
    let allocsize (_bs, m) = 
      2 * List.length m

    (* Nombre de cases utilisées *)
    let busyness (_bs, m) = 
      List.fold_left (fun a b -> a + b) 0 (List.map (fun (_, b) -> if b = _0 then 1 else 0) m) 

    (* Construire une mémoire vide *)
    let clear bs = 

      let rec aux i =
        if i > 0 then
          (i, _0) :: aux (i-1)
        else
          []
      in
      
      (bs, aux(pow2 bs))

    (* Lire une valeur *)
    let read (bs, m) addr = 
    
      if addr < (pow2 bs) then
        get_assoc addr m _0
      else
        raise OutOfBound

    (* Écrire une valeur *)
    let write (bs, m) addr x =
        
        if addr < (pow2 bs) then
         (bs, set_assoc addr m x) 
        else
          raise OutOfBound

end
