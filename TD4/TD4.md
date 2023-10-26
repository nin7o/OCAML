```ocaml

module type CollectionType = 
sig 
    type 'a t 
    exception CollectionVide 
    val vide : 'a t 
    val est_vide : 'a t -> bool 
    val ajouter : 'a -> 'a t -> 'a t 
    val enlever : 'a t -> ('a, 'a t)
end





```
