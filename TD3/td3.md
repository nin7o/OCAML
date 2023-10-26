
### Exercice 1

```ocaml
type 'a arbre = Noeud of bool * ('a branche list)
and 'a branche = 'a * 'a arbre

(* branche b *)
let bs = ('s', Noeud(true, [] ))
let bt = ('t', Noeud(true, [] ))
let ba = ('a', Noeud (false, [bs;bt] ))
let bb = ('b', Noeud (false, [ba] ))
```

### Exercice 2 

```ocaml

let rec appartient lc (Noeud (b, lb)) = 
    match lc, lb with
    [],_ -> b
    | _,[] -> false 
    | c::qlc,(c1,b1)::qlb -> 
        if c = c1 then
            appartient qlc b1
        else if c < c1 then
            false
        else
            appartient lc (Noeud (b,qlb))

```

### Exercice 3

