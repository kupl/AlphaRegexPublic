open Vocab

(***************************************)
(* the language of regular expressions *)
(***************************************)
type alphabet = A | B | NonInputSymbol
type str = alphabet list
type example = str

type exp = 
  | ALPHA of alphabet 
  | OR of exp * exp
  | CONCAT of exp * exp
  | CLOSURE of exp
  | OZ of exp (* ?: zero or one *)
  | HOLE of int
and pgm = exp

(* generating a unique hole *)
let cnt = ref 0
let new_hole : unit -> exp
= fun () -> cnt := !cnt + 1; HOLE (!cnt)

let alpha2char : alphabet -> string
=fun a -> match a with | A -> "0" | B -> "1" | NonInputSymbol -> "X"

let char2alpha c = 
  match c with
  | '0' -> A
  | '1' -> B
  | _ -> raise (Failure "Error: unsupported alphabet")

let str2str : str -> string
=fun s -> List.fold_left (fun s a -> s ^ (alpha2char a)) "" s
 
let rec exp2str_mod_hole e = 
  match e with
  | ALPHA a -> alpha2char a
  | OR (e1,e2) -> "("^exp2str_mod_hole e1 ^ "|" ^ exp2str_mod_hole e2^")"
  | CONCAT (e1,e2) -> "("^exp2str_mod_hole e1 ^ "" ^ exp2str_mod_hole e2^")"
  | CLOSURE e -> "("^exp2str_mod_hole e^"*)"
  | OZ e -> "("^exp2str_mod_hole e^"?)"
  | HOLE n -> "H"

let rec exp2str e =
  let rec e2s e i (* i for initial expression *)
  = match e with
    | ALPHA a -> alpha2char a
    | OR (e1,e2) -> 
      if OR (e1,e2) = i 
        then exp2str e1 ^ "+" ^ e2s e2 i
      else
        "("^e2s e1 i^ "+" ^ e2s e2 i^")"
    | CONCAT (e1,e2) -> ""^(e2s e1 i) ^ "" ^ (e2s e2 i)^""
    | CLOSURE e ->
      (match e with
       | CONCAT _ -> "("^e2s e i^")*"
       | _ -> "" ^ e2s e i^"*")
    | OZ e->
      (match e with
      | CONCAT _ -> "("^e2s e i^")?"
      | _ -> ""^e2s e i^"?")
    | HOLE n -> "H" ^string_of_int n
  in e2s e e 


let exp2str_w_outset : exp -> string
=fun exp ->
  try
  let outset = 
    match exp with 
    | CLOSURE _ -> "*"
    | OZ _ -> "?"
    | OR _ -> "|"
    | CONCAT _ -> "-"
    | _ -> "" in
   exp2str exp ^ " : " ^ outset
 with _ -> raise (Failure "exp2str")

(* regular expression representation for module Str *)
let rec exp2str_e : exp -> string
=fun exp ->
  match exp with
  | ALPHA a -> "\(" ^ alpha2char a ^ "\)"
  | OR (e1,e2) -> "\("^exp2str_e e1 ^ "\|" ^ exp2str_e e2^"\)"
  | CONCAT (e1,e2) -> "\("^exp2str_e e1 ^ "" ^ exp2str_e e2^"\)"
  | CLOSURE e -> "\("^exp2str_e e^"*\)"
  | OZ e -> "\("^exp2str_e e^"?\)"
  | HOLE n -> "H"^string_of_int n

let pp : exp -> unit
=fun exp -> print_endline (exp2str exp); flush stdout

let rec level : exp -> int
=fun e ->
  match e with
  | ALPHA _
  | HOLE _ -> 1
  | CLOSURE e
  | OZ e -> level e + 1
  | CONCAT (e1,e2)
  | OR (e1,e2) -> max (level e1) (level e2) + 1

let cost : exp -> int
=fun e ->
  let rec cost e = 
    match e with
    | OR (ALPHA A, ALPHA B) -> if !mode = IDIOM then 20
                               else cost (ALPHA A) + cost (ALPHA B) + 30
    | ALPHA _ -> 20 (*20*)
    | OR (e1,e2) -> cost e1 + cost e2 + 30 (*30*)
    | CONCAT (e1,e2) -> cost e1 + cost e2 + 5 (*5*)
    | CLOSURE e -> cost e + 20 (*20*)
    | OZ e -> cost e + 20 (*20*)
    | HOLE _ -> 100 (*100*) in
  let rec get_depth e = 
    match e with
    | ALPHA _ -> 1
    | CONCAT (e1,e2) 
    | OR (e1,e2) -> max (get_depth e1) (get_depth e2)
    | CLOSURE e -> get_depth e + 1
    | OZ e -> get_depth e + 1
    | HOLE _ -> 1 in
    cost e + int_of_float (float_of_int 10 ** float_of_int (get_depth e - 2))

let rec opt : exp -> exp 
=fun e ->
  match e with
  | CLOSURE (CLOSURE e) -> CLOSURE (opt e)                              (* e** -> e* *)
  | CLOSURE (OZ e) -> CLOSURE (opt e)                                   (* e?* -> e* *)
  | CLOSURE (CONCAT (e1, CLOSURE e2)) when e1 = e2 -> CLOSURE (opt e1)  (* (ee* )* -> e* *)
  | CLOSURE (CONCAT (CLOSURE e1, e2)) when e1 = e2 -> CLOSURE (opt e1)  (* (e*e)* -> e* *)
  | CLOSURE (CONCAT (e1, OZ e2)) when e1 = e2 -> CLOSURE (opt e1)       (* (ee?)* -> e* *)
  | CLOSURE (CONCAT (OZ e1, e2)) when e1 = e2 -> CLOSURE (opt e1)       (* (e?e)* -> e* *)
  | OZ (CLOSURE e) -> CLOSURE (opt e)                                   (* e*? -> e* *)
  | OZ (OZ e) -> OZ (opt e)                                             (* e?? -> e? *)
  | OZ (CONCAT (e1, CLOSURE e2)) when e1 = e2 -> CLOSURE (opt e1)       (* (ee* )? -> e* *)
  | OZ (CONCAT (CLOSURE e1, e2)) when e1 = e2 -> CLOSURE (opt e1)       (* (e*e )? -> e* *)
  | OR (e1, e2) when e1 = e2 -> opt e1                                  (* e|e -> e *)
  | OR (e1, CLOSURE e2) when e1 = e2 -> CONCAT (opt e1, CLOSURE (opt e2)) (* e+e* -> ee* *)
  | OR (CLOSURE e1, e2) when e1 = e2 -> CONCAT (opt e1, CLOSURE (opt e2)) (* e*+e -> ee* *)
  | OR (e1, OR (e2, OZ e3)) when e1 = e3 -> OR (opt e2, OZ (opt e1))              (* added : e1+(e2+e1?) -> e2+e1? *)
  | OR (e1, OR (e2, CLOSURE e3)) when e1 = e3 -> OR (opt e2, CLOSURE (opt e1))    (* added : e1+(e2+e1* ) -> e2+e1* *)
  | OR (e1, OR (OZ e2, e3)) when e1 = e2 -> OR (OZ (opt e1), opt e3)              (* added : e1+(e1?+e2) -> e1?+e2 *)
  | OR (e1, OR (CLOSURE e2, e3)) when e1 = e2 -> OR (CLOSURE (opt e1), opt e3)    (* added : e1+(e1*+e2) -> e1*+e2 *)
  | OR (OZ e1, OR (e2, e3)) when e1 = e2 -> OR (OZ (opt e1), opt e2)              (* added : e1?+(e1+e2) -> e1?+e2 *)
  | OR (CLOSURE e1, OR (e2, e3)) when e1 = e2 -> OR (CLOSURE (opt e1), opt e2)    (* added : e1*+(e1+e2) -> e1*+e2 *)
  | OR (e1, e2) when e1=ALPHA B && e2=ALPHA A -> OR (opt e2, opt e1)              (* added : (b+a) -> (a+b) *) 
  | OR (e1, OR (e2, e3)) when e1 = e2 -> OR (opt e1, opt e3)            (* e|e|e' -> e|e' *)
  | OR (e1, OR (e2, e3)) when e1 = e3 -> OR (opt e1, opt e2)            (* e|e'|e -> e|e' *)
  | OR (OZ e1, CLOSURE e2) when e1 = e2 -> CLOSURE (opt e2)             (* e?|e* -> e* *)
  | OR (CLOSURE e1, OZ e2) when e1 = e2 -> CLOSURE (opt e1)             (* e*|e? -> e* *)
  | OR (OZ e1, e2) when e1 = e2 -> OZ (opt e1)                          (* e|e? -> e? *) 
  | OR (e1, OZ e2) when e1 = e2 -> OZ (opt e1)                          (* e?|e -> e? *)
  | CONCAT (CLOSURE e1, CLOSURE e2) when e1 = e2 -> CLOSURE (opt e1)    (* e*e* -> e* *)
  | CONCAT (CLOSURE e1, OZ e2) when e1 = e2 -> CLOSURE (opt e1)         (* e*e? -> e* *)
  | CONCAT (OZ e1, CLOSURE e2) when e1 = e2 -> CLOSURE (opt e1)         (* e?e* -> e* *)
  | CONCAT (CLOSURE e1, CONCAT (CLOSURE e2, e3)) when e1 = e2 -> CONCAT (CLOSURE (opt e1), opt e3) (* e*e*e' -> e*e' *)
  | CONCAT (OZ e1, CONCAT (CLOSURE e2, e3)) when e1 = e2 -> CONCAT (CLOSURE (opt e1), opt e3) (* e?e*e' -> e*e' *)
  | CONCAT (CLOSURE e1, CONCAT (OZ e2, e3)) when e1 = e2 -> CONCAT (CLOSURE (opt e1), opt e3) (* e*e?e' -> e*e' *)
  | OR (e1, e2) -> OR (opt e1, opt e2)                              
  | CONCAT (e1, e2) -> CONCAT (opt e1, opt e2)
  | CLOSURE e -> CLOSURE (opt e)
  | OZ e -> OZ (opt e)
  | _ -> e

let normalize : exp -> exp 
=fun e -> 
  let _ = Profiler.start_event "normalize" in 
  let res = fix opt e in
  let _ = Profiler.finish_event "normalize" in
    res

(* equivalence modular holes *)
let rec eq_mod_hole : exp -> exp -> bool
=fun e1 e2 ->
  let b = 
    match e1, e2 with
    | HOLE _, HOLE _ -> true
    | ALPHA a, ALPHA b -> a = b
    | OR (e1,e2), OR (e1',e2') 
    | CONCAT (e1,e2), CONCAT (e1',e2') -> eq_mod_hole e1 e1' && eq_mod_hole e2 e2'
    | CLOSURE e, CLOSURE e' -> eq_mod_hole e e'
    | OZ e, OZ e' -> eq_mod_hole e e'
    | _ -> false in
    b

let rec replace_all_holes : exp -> exp -> exp
=fun exp exp' ->
  match exp with
  | HOLE _ -> exp'
  | ALPHA _ -> exp
  | OR (e1,e2) -> OR (replace_all_holes e1 exp', replace_all_holes e2 exp')
  | CONCAT (e1,e2) -> CONCAT (replace_all_holes e1 exp', replace_all_holes e2 exp')
  | CLOSURE e -> CLOSURE (replace_all_holes e exp')
  | OZ e -> OZ (replace_all_holes e exp')

let sigma_star = CLOSURE (OR (ALPHA A, ALPHA B))
