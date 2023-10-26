(*  Module qui permet la décomposition et la recomposition de données **)
(*  Passage du type t1 vers une liste d'éléments de type t2 (décompose) **)
(*  et inversement (recopose).**)
module type DecomposeRecompose =
sig
  (*  Type de la donnée **)
  type mot
  (*  Type des symboles de l'alphabet de t1 **)
  type symbole

  val decompose : mot -> symbole list
  val recompose : symbole list -> mot
end

module DRString : DecomposeRecompose =
struct
  type mot = string
  type symbole = char

  let decompose s =
    let rec decompose i accu =
    if i < 0 then accu
    else decompose (i-1) (s.[i]::accu)
    in decompose (String.length s - 1) []

  let%test _ = decompose "" = []
  let%test _ = decompose "a" = ['a']
  let%test _ = decompose "aa" = ['a';'a']
  let%test _ = decompose "ab" = ['a';'b']
  let%test _ = decompose "abcdef" = ['a'; 'b'; 'c'; 'd'; 'e'; 'f']

  let recompose lc =
  List.fold_right (fun t q -> String.make 1 t ^ q) lc ""

  let%test _ = recompose [] = ""
  let%test _ = recompose ['a'] = "a"
  let%test _ = recompose ['a';'a'] = "aa"
  let%test _ = recompose ['a';'b'] = "ab"
  let%test _ = recompose ['a'; 'b'; 'c'; 'd'; 'e'; 'f'] = "abcdef"

end

module DRNat : DecomposeRecompose =
struct
  type mot = int
  type symbole = int 

  let rec decompose mot = 
     if mot < 10 then
       [mot]
    else
      decompose(mot / 10) @ [(mot mod 10)]

  let%test _ = decompose 0 = [0]
  let%test _ = decompose 12 = [1;2]
  let%test _ = decompose 34 = [3;4]
  let%test _ = decompose 123456 = [1; 2; 3; 4; 5; 6]
  
  let recompose list = 
    List.fold_left (fun a b -> a * 10 + b) 0 list

  let%test _ = recompose [1] = 1
  let%test _ = recompose [2;5] = 25
  let%test _ = recompose [4;6] = 46
  let%test _ = recompose [1; 2; 3; 4; 5; 6] = 123456 

end 
