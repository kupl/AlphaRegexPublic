type mode =
  | DEFAULT
  | IDIOM

let mode = ref DEFAULT

(* fixpoint operator *)
let rec fix f x = 
  let x' = f x in
    if x = x' then x else fix f x'

(* function composition *)
let (>>>) f g = fun x -> g (f x)

let fst (a,_) = a
let snd (_,a) = a

(* get the sublist from s to e *)
let list_get l s e =
  snd (
    List.fold_left (fun (i,l) a ->
      if s <= i && i <= e then (i+1,l@[a]) else (i+1,l)
    ) (0,[]) l)


(* from sparrow-public *)
let flip f = fun y x -> f x y

let list_fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
= fun f list init ->
  List.fold_left (flip f) init list

let list2set l = list_fold BatSet.add l BatSet.empty

let rec remove_dups lst 
= match lst with
  | [] -> []
  | h::t -> h::(remove_dups (List.filter (fun x -> x<>h) t)) 
