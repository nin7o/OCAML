open Dr

(* Exception levée quand le code ne peut pas être déchiffré avec l'arbre de chiffrement *)
exception CodeNonValide
(* Exception levée quand la donnée ne peut pas être chiffrée avec l'arbre de chiffrement *)
exception DonneeNonValide

module Arbre =
struct

  (* Arbre de chiffrement : arbre n-aire avec données de type 'b dans les branches et 'f dans les feuilles *)
  type ('b,'f) arbre_chiffrement = 
    | Noeud of ('b * ('b, 'f) arbre_chiffrement) list
    | Lettre of 'f

  (* Arbre de chiffrement de la figure 1 du sujet *)
  let arbre1 =
    Noeud (
        [(1,Noeud ([(1,Lettre 'e');(2,Lettre 'b')]) );
         (2,Noeud (
                [(1,Noeud ([(2,Lettre 'c');(3,Lettre 'f')]));
                 (2,Lettre 'd')]));
         (3,Lettre 'a')]
      )


  (* Arbre de chiffrement de la figure 2 du sujet *)
  let arbre2 =
    Noeud ([(1,Lettre 'a');(2,Lettre 'b'); (3,Lettre 'c')])


  (* Arbre de chiffrement de la figure 3 du sujet *)
  let arbre3 =
    Noeud([
          ('a', Noeud([('u',Lettre 'a');('a', Lettre 'b')]));
          ('e', Lettre 'c');
          ('i', Noeud([('o',Lettre 'd');('u',Lettre 'e');('e',Lettre 'f')]))
      ])


  (* Arbre de chiffrement de la figure 4 du sujet *)
  let arbre4 =
    Noeud([
          ('a',Lettre 'c');
          ('b', Lettre 'd');
          ('c', Lettre 'e');
          ('d', Lettre 'f');
          ('e', Lettre 'a');
          ('f', Lettre 'b')
      ])


  (*  get_branche : 'b ->  ('b,'f) arbre_chiffrement -> ('b,'f) arbre_chiffrement option**)
  (*  Cherche la branche qui est étiquetée par e et renvoie le sous-arbre associé *)
  let get_branche e arbre = 
    match arbre with 
    | Lettre(_) -> failwith "aucune branche étiquetée"
    | Noeud(list) -> 
        List.assoc_opt e list 

  let%test _ = get_branche 1 arbre1 = Some (Noeud ([(1,Lettre 'e');(2,Lettre 'b')]))
  let%test _ = get_branche 2 arbre2 = Some (Lettre 'b')
  let%test _ = get_branche 'i' arbre3 = Some (Noeud([('o',Lettre 'd');('u',Lettre 'e');('e',Lettre 'f')]))
  let%test _ = get_branche 'd' arbre4 = Some (Lettre 'f')


  (*  dechiffrer : 'b list -> ('b,'f) arbre_chiffrement -> 'f list **)
  (*  transforme un code (sous forme de liste des symboles de l'alphabet des code) à l'aide d'un arbre **)
  (*  renvoie la donnée déchiffrée, sous forme de liste de symboles de l'alphabet des données **)
  (*  Exception CodeNonValide : si le code ne peut pas être déchiffré avec l'arbre de chiffrement**)
  let dechiffrer list arbre = 

    let rec decode_1_symbole code arbre arbre_init = 
      match code, arbre with
      | _, Lettre e -> ([e], code, arbre_init)
      | symbole::reste_code,  arbre ->
          (match get_branche symbole arbre with
          | Some  branche -> decode_1_symbole reste_code branche arbre_init
          | None -> raise CodeNonValide)
      | _, _ -> raise CodeNonValide

    in

    let rec decode_tous_symboles code arbre decode = 
      match code with 
      | [] -> decode
      | _ ->  
          let (e, code_restant, arbre_init) = decode_1_symbole code arbre arbre in 
          decode_tous_symboles code_restant arbre_init (decode@e)
    in 

    decode_tous_symboles list arbre []

           


  let%test _ = dechiffrer [1;2;3;2;1;2] arbre1 = ['b';'a';'c']
  let%test _ = dechiffrer [1;2;3] arbre1 = ['b';'a']
  let%test _ = try let _ = dechiffrer [3;2;1;3;2;1] arbre1 in false with CodeNonValide -> true

  let%test _ = dechiffrer [1;2;3;2;1;2] arbre2 = ['a';'b';'c';'b';'a';'b']
  let%test _ = dechiffrer [1;2;3] arbre2 = ['a';'b';'c']
  let%test _ = dechiffrer [3;2;1;3;2;1] arbre2 = ['c';'b';'a';'c';'b';'a']
  let%test _ = try let _ = dechiffrer [4;5;7] arbre2 in false with CodeNonValide -> true

  let%test _ = dechiffrer ['a';'u'] arbre3 = ['a']
  let%test _ = dechiffrer ['a';'a'] arbre3 = ['b']
  let%test _ = dechiffrer ['e'] arbre3 = ['c']
  let%test _ = dechiffrer ['a';'a';'a';'u';'e'] arbre3 = ['b';'a';'c']

  let%test _ = dechiffrer ['f';'e';'a'] arbre4 = ['b';'a';'c']
 

  (*  arbre_to_list : ('b,'f) arbre_chiffrement -> ('f * 'b list ) list **)
  (*  Converti un arbre de chiffrement en une liste associative (symbole de l'alphabet des données, code - liste de symboles de l'alphabet des codes)**)
  let  arbre_to_liste arbre =

    let rec aux (list_arbre) list_chemin list_totale = 
      match list_arbre with 
      | [] -> []
      | (e, branche)::reste_arbres ->
          (match branche with
           | Some Lettre(symbole) -> [(symbole, list_chemin@[e])]@list_totale 
           | Some Noeud(list) -> aux (reste_arbres@list) (list_chemin@[e]) list_totale
           | None -> failwith "error" 
          )
      in
    
    match arbre with
    | Some Noeud(list) -> aux list [] []
    | _ -> failwith "arbre mal défini"
       

  (*  [eq_perm l l'] retourne true ssi [l] et [l']
      sont égales à à permutation près (pour (=)).
      [l'] ne doit pas contenir de doublon. *)
  let eq_prem l l' =
    List.length l = List.length l' && List.for_all (fun x -> List.mem x l) l'

(*  let%test _ = eq_prem (arbre_to_liste arbre2) ['a',[1]; 'b',[2]; 'c',[3]]

  let%test _ =
    eq_prem
      (arbre_to_liste arbre3)
      ['a',['a';'u']; 'b',['a';'a']; 'c',['e']; 'd',['i';'o']; 'e',['i';'u'];
       'f',['i';'e']]

  let%test _ =
    eq_prem
      (arbre_to_liste arbre4)
      ['a',['e']; 'b',['f']; 'c',['a']; 'd',['b']; 'e',['c']; 'f',['d']]
 *)

  (*  chiffrer : 'f list -> ('b,'f) arbre_chiffrement -> 'b list **)
  (*  transforme une donnée (sous forme de liste de symboles de l'alphabet des données)  **)
  (*  renvoie le code associé à la donnée, pour l'abre, sous forme de liste de symboles de l'alphabet des codes **)
  (*  Exception DonneeNonValide : si la donnéee ne peut pas être chiffrée avec l'arbre de chiffrement **)
  let chiffrer   = fun _ -> assert false

(*  let%test _ = chiffrer ['b';'a';'c'] arbre1 = [1;2;3;2;1;2]
  let%test _ = chiffrer ['b';'a'] arbre1 = [1;2;3]

  let%test _ = chiffrer ['a';'b';'c';'b';'a';'b'] arbre2 = [1;2;3;2;1;2]
  let%test _ = chiffrer ['a';'b';'c'] arbre2 = [1;2;3]
  let%test _ = chiffrer ['c';'b';'a';'c';'b';'a'] arbre2 = [3;2;1;3;2;1]
  let%test _ = try let _ = chiffrer ['d';'a';'b'] arbre2 in false with DonneeNonValide -> true 
  let%test _ = try let _ = chiffrer ['z';'u';'t'] arbre2 in false with DonneeNonValide -> true

  let%test _ = chiffrer ['b';'a';'c'] arbre3 = ['a';'a';'a';'u';'e']

  let%test _ = chiffrer ['b';'a';'c'] arbre4 = ['f';'e';'a']
 *)

  (*****************************************)
  (*  BONUS                               **)
  (*****************************************)

  (*  fold :  TO DO
   **)
  let rec fold = fun _ -> assert false

  (*  nb_symboles : ('b,'f) arbre_chiffrement : ('b,'f) arbre_chiffrement -> int
      Fonction qui calcule le nombre de symboles de l'alphabet des données présentent dans un arbre de chiffrement
      Paramètre : l'arbre de chiffrement
      Retour : le nombre de synboles de l'alphabet des données
   **)
  let nb_symboles = fun _ -> assert false

(*  let%test _ = nb_symboles arbre1 = 6
  let%test _ = nb_symboles arbre2 = 3
  let%test _ = nb_symboles arbre3 = 6
  let%test _ = nb_symboles arbre4 = 6
 *)

  (*  symboles :  ('b,'f) arbre_chiffrement -> 'f list
      Fonction qui renvoie la liste des symboles de l'alphabet des données présents dans un arbre de chiffrement
      Paramètre : l'arbre de cryage
      Retour : la liste des symboles
   **)
  let symboles = fun _ -> assert false

(*  let%test _ = eq_prem (symboles arbre1) ['a'; 'b'; 'c'; 'd'; 'e'; 'f']
  let%test _ = eq_prem (symboles arbre2) ['a'; 'b'; 'c']
  let%test _ = eq_prem (symboles arbre3) ['a'; 'b'; 'c'; 'd'; 'e'; 'f']
  let%test _ = eq_prem (symboles arbre4) ['a'; 'b'; 'c'; 'd'; 'e'; 'f']
 *)

  (*  arbre_to_liste_2 :  ('b,'f) arbre_chiffrement -> ('f * 'b list ) list **)
  (*  Converti un arbre de chiffrement en une liste associative (symbole de l'alphabet des données, code - liste de symboles de l'alphabet des codes)**)
  let arbre_to_liste_2  = fun _ -> assert false

(*  let%test _ =
    eq_prem
      (arbre_to_liste_2 arbre1)
      ['a',[3]; 'b',[1;2]; 'c',[2;1;2]; 'd',[2;2]; 'e',[1;1]; 'f',[2;1;3]]

  let%test _ = eq_prem (arbre_to_liste_2 arbre2) ['a',[1]; 'b',[2]; 'c',[3]]

  let%test _ =
    eq_prem
      (arbre_to_liste_2 arbre3)
      ['a',['a';'u']; 'b',['a';'a']; 'c',['e']; 'd',['i';'o']; 'e',['i';'u'];
       'f',['i';'e']]

  let%test _ =
    eq_prem
      (arbre_to_liste_2 arbre4)
      ['a',['e']; 'b',['f']; 'c',['a']; 'd',['b']; 'e',['c']; 'f',['d']]
 *)

end

module Chiffrement (DRDonnee : DecomposeRecompose) (DRCode : DecomposeRecompose)=
struct

  type donnee = TODO
  type symbole_donnee = TODO
  type symbole_code = TODO
  type code = TODO

  (* dechiffrer : code -> (symbole_code,symbole_donnee) arbre_chiffrement -> donnee
     Déchiffre un code en utilisant l'abre de chiffrement
     Paramètre code : le code à déchiffrer
     Paramètre arbre : l'arbre de chiffrement
     Retour : la donnée déchiffrée
     Exception CodeNonValide : si le code ne peut pas être déchiffré avec l'arbre de chiffrement
   *)
  let dechiffrer = fun _ -> assert false

  (* chiffrer : donnee -> (symbole_code,symbole_donnee) arbre_chiffrement -> code
     chiffre une donnée à l'aide d'un arbre de chiffrement
     Paramètre donnee : la donnée à chiffrer
     Paramètre arbre : l'arbre de chiffrement
     Retour : le code associé à la donnée
     Exception DonneeNonValide : si la donnéee ne peut pas être chiffrée avec l'arbre de chiffrement 
   *)
  let chiffrer = fun _ -> assert false

end
