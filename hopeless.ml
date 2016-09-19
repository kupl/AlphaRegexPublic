open Lang
open Vocab

(* expression that consists of only holes *)
let rec only_hole : exp -> bool
= fun e ->
  match e with
  | ALPHA _ -> false
  | HOLE _ -> true
  | OR (e1,e2)
  | CONCAT (e1,e2) -> only_hole e1 && only_hole e2
  | CLOSURE e'
  | OZ e' -> only_hole e'

let not_only_hole : exp -> bool
= fun e -> not (only_hole e)

let rec unroll : exp -> exp 
= fun e ->
  match e with
  | ALPHA _ -> e 
  | OR (e1,e2) -> OR (unroll e1, unroll e2)
  | CONCAT (e1,e2) -> CONCAT (unroll e1, unroll e2)
  | CLOSURE e' when not_only_hole e' -> CONCAT (e', CONCAT (e', CLOSURE e')) 
  | OZ e' when not_only_hole e' -> unroll e' 
  | _ -> e

let rec split : exp -> exp list 
= fun exp ->
  match exp with
  | ALPHA _ -> [exp]
  | OR (e1,e2) -> (split e1)@(split e2)
  | CONCAT (e1,e2) -> 
    let e1_lst = split e1 in
    let e1res = List.fold_left (fun acc e1' -> acc@[CONCAT (e1',e2)]) [] e1_lst in
    let e2_lst = split e2 in
    let e2res = List.fold_left (fun acc e2' -> acc@[CONCAT (e1,e2')]) [] e2_lst in
    remove_dups e1res@e2res
  | CLOSURE _
  | OZ _
  | HOLE _ -> [exp]

let rec redundant : (exp -> example -> bool) -> exp -> example list -> example list -> bool
= fun run exp pos_examples neg_examples ->
  let exp = unroll exp in
  let elst = remove_dups (split exp) in
  let elst = List.map (fun e -> replace_all_holes e sigma_star) elst in
  let b = List.exists (fun e -> List.for_all (fun str -> not (run e str)) pos_examples) elst in
  b

(* detect hopeless hypotheses that are guaranteed to fail *)
let hopeless : (exp -> example -> bool) -> exp -> example list -> example list -> bool
=fun run exp pos_examples neg_examples -> 
  let exp_over  = replace_all_holes exp (CLOSURE (OR (ALPHA A, ALPHA B))) in
  let exp_under = replace_all_holes exp (ALPHA NonInputSymbol) in
    if List.exists (fun str -> not (run exp_over str)) pos_examples then true
    else if List.exists (fun str -> run exp_under str) neg_examples then true
    else if redundant run exp pos_examples neg_examples then true
    (* true implies fail to satisfy user's intention *)
    else false

