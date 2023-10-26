(* Exercice 3 *)
module type Expression = sig
  (* Type pour représenter les expressions *)
  type exp


  (* eval : exp -> int
    Evalue l'expression donnée et renvoie un entier
    Paramètres : exp : l'expression à évaluer
    Précondition : aucune *)
  val eval : exp -> int
end

(* Exercice 4 *)

module ExpAvecArbreBinaire : Expression =  
  struct

  type op = Moins | Plus | Mult | Div
  type exp = Binaire of exp * op * exp | Entier of int

  let rec eval expression =
    match expression with
    | Entier(n) -> n
    | Binaire(e1, op, e2) ->
        let v1 = eval e1 in 
        let v2 = eval e2 in 
        match op with
        | Moins -> v1 - v2
        | Plus -> v1 + v2
        | Mult -> v1 * v2
        | Div -> v1 / v2 

  let expr1 = Binaire(Entier(3), Plus, Entier(4)) 
  let expr2 = Binaire(expr1, Moins, Entier(12))

  let%test _ = eval expr2 = -5
end

(* Exercice 5 *)

module ExpAvecArbreNaire : Expression =
struct
  (* Linéarisation des opérateurs binaire associatif gauche et droit *)
  type op = Moins | Plus | Mult | Div
  type exp = Naire of op * exp list | Valeur of int

  
  (* bienformee : exp -> bool *)
  (* Vérifie qu'un arbre n-aire représente bien une expression n-aire *)
  (* c'est-à-dire que les opérateurs d'addition et multiplication ont au moins deux opérandes *)
  (* et que les opérateurs de division et soustraction ont exactement deux opérandes.*)
  (* Paramètre : l'arbre n-aire dont ont veut vérifier si il correspond à une expression *)
  let rec bienformee expression = 
 
    let _combine_and acc element =
      acc && element
    in 

    match expression with
    | Valeur(_) -> true 
    | Naire(op, list) ->
        let l = List.length list in 
        if op = Plus || op = Mult then
          (l >= 2) && List.fold_left (&&) true (List.map bienformee list)
        else
          (l = 2) && List.fold_left (&&) true (List.map bienformee list) 


  let en1 = Naire (Plus, [ Valeur 3; Valeur 4; Valeur 12 ])
  let en2 = Naire (Moins, [ en1; Valeur 5 ])
  let en3 = Naire (Mult, [ en1; en2; en1 ])
  let en4 = Naire (Div, [ en3; Valeur 2 ])
  let en1err = Naire (Plus, [ Valeur 3 ])
  let en2err = Naire (Moins, [ en1; Valeur 5; Valeur 4 ])
  let en3err = Naire (Mult, [ en1 ])
  let en4err = Naire (Div, [ en3; Valeur 2; Valeur 3 ])

  let%test _ = bienformee en1
  let%test _ = bienformee en2
  let%test _ = bienformee en3
  let%test _ = bienformee en4
  let%test _ = not (bienformee en1err)
  let%test _ = not (bienformee en2err)
  let%test _ = not (bienformee en3err)
  let%test _ = not (bienformee en4err)

  (* eval : exp-> int *)
  (* Calcule la valeur d'une expression n-aire *)
  (* Paramètre : l'expression dont on veut calculer la valeur *)
  (* Précondition : l'expression est bien formée *)
  (* Résultat : la valeur de l'expression *)
  let  eval_bienformee expression = 

    let mult a b =
      a * b
    in

    let rec aux expr = 
      match expr with
      | Valeur(a) -> a
      | Naire(op, list) -> 
          let list_eval = List.map aux list in 
          match op with
          | Plus -> List.fold_left (+) 0 list_eval
          | Mult -> List.fold_left mult 1 list_eval
          | Div -> (match list_eval with
                    | e1::e2::_tl -> e1 / e2   
                    | _ -> failwith "malformée") 
          | Moins -> (match list_eval with
                    | e1::e2::_tl -> e1 - e2
                    | _ -> failwith "malformée")

    in 

    aux expression
        

  let%test _ = eval_bienformee en1 = 19
  let%test _ = eval_bienformee en2 = 14
  let%test _ = eval_bienformee en3 = 5054
  let%test _ = eval_bienformee en4 = 2527

  (* Définition de l'exception Malformee *)
  exception Malformee

  (* eval : exp-> int *)
  (* Calcule la valeur d'une expression n-aire *)
  (* Paramètre : l'expression dont on veut calculer la valeur *)
  (* Résultat : la valeur de l'expression *)
  (* Exception  Malformee si le paramètre est mal formé *)
  let eval expression = 
    if bienformee expression then
      eval_bienformee expression
    else 
      raise Malformee

  let%test _ = eval en1 = 19
  let%test _ = eval en2 = 14
  let%test _ = eval en3 = 5054
  let%test _ = eval en4 = 2527

  let%test _ =
    try
      let _ = eval en1err in
      false
    with Malformee -> true

  let%test _ =
    try
      let _ = eval en2err in
      false
    with Malformee -> true

  let%test _ =
    try
      let _ = eval en3err in
      false
    with Malformee -> true

  let%test _ =
    try
      let _ = eval en4err in
      false
    with Malformee -> true


end
