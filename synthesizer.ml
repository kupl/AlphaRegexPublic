open Options
open Lang
open Vocab
open Hopeless

(*************************************)
(* examples and consistency checking *)
(*************************************)

let iter = ref 0
let t0 = ref (Sys.time ())
let t_total = Sys.time ()

let print_examples examples = 
  List.iter (fun str ->
    print_endline (str2str str)
  ) examples

let rec run : exp -> example -> bool
=fun exp example ->
  let _ = Profiler.start_event "Run" in
  let b = 
    let regexp = Str.regexp (exp2str_e exp ^ "\.") in
    let str = str2str example ^ "." in
      Str.string_match regexp str 0 in
  let _ = Profiler.finish_event "Run" in
    b

let consistent : exp -> example list -> example list -> bool
=fun exp pos_examples neg_examples -> 
  let p = List.for_all (fun str -> run exp str) pos_examples in
  let n = List.for_all (fun str -> not (run exp str)) neg_examples in
    if !verbose >= 1 then begin
      print_endline ("- Consistency checking for " ^ exp2str exp); 
      print_endline ("  positive : " ^ string_of_bool p ^ ", negative : " ^ string_of_bool n) 
      end
    else
      ();
    (if not n && !verbose >= 1 then 
        print_endline ("  " ^ str2str (List.find (fun str -> run exp str) neg_examples))
    else ());
    p && n

let needless : exp -> example list -> example list -> bool
=fun exp pos_examples neg_examples ->
  let is_outset_zeroone exp = match exp with OZ _ -> true | _ -> false in
(*  let pos_examples_hd_arbitrary = 
    List.exists (fun example -> List.hd example = A) pos_examples &&
    List.exists (fun example -> List.hd example = B) pos_examples in
  let hd_is_closure_of_single_alpha exp = 
      match exp with
      | CONCAT (CLOSURE (ALPHA _), _) -> true
      | CLOSURE (CONCAT (CLOSURE (ALPHA _), _)) -> true
      | _ -> false in
*)    if (* (pos_examples_hd_arbitrary && hd_is_closure_of_single_alpha exp) || *)
       is_outset_zeroone exp 
    then true
    else false

(*************************************)
(* available, holes, subst           *)
(*************************************)

let available : unit -> exp list
=fun () ->
  let availst
  = [ALPHA A; ALPHA B; OR (new_hole(), new_hole()); 
    CONCAT (new_hole(), new_hole()); CLOSURE (new_hole()); OZ (new_hole())] in
  if !mode = IDIOM 
    then availst@[OR (ALPHA A, ALPHA B)]
  else availst

let rec holes : exp -> exp list
=fun e ->
  match e with
  | HOLE h -> [e]
  | OR (e1,e2)
  | CONCAT (e1,e2) -> (holes e1)@(holes e2)
  | CLOSURE e -> holes e
  | OZ e -> holes e
  | _ -> []

(* replace hole inside e by e' *)
let rec subst : exp -> exp -> exp -> exp
=fun e e' hole ->
  match hole with
  | HOLE h ->
    begin
      match e with
      | HOLE h' when h = h' -> e' (* replace the hole with e' *)
      | OR (e1,e2) -> OR (subst e1 e' (HOLE h),subst e2 e' (HOLE h))
      | CONCAT (e1,e2) -> CONCAT (subst e1 e' (HOLE h),subst e2 e' (HOLE h)) 
      | CLOSURE e -> CLOSURE (subst e e' (HOLE h))
      | OZ e -> OZ (subst e e' (HOLE h))
      | _ -> e
    end
  | _ -> raise (Failure "subst: hole is not specified")

(* A?(AB)*B? *)
let solution = CONCAT (CONCAT (OZ (ALPHA B), CLOSURE (CONCAT (ALPHA A, ALPHA B))), OZ (ALPHA A))
let init () = 
  let hole = new_hole() in
    (hole, Some hole)


(*************************************)
(*             Worklist              *)
(*************************************)
type work = exp * (exp option)

module Worklist = struct
  module OrderedType = struct
    type t = work
    let compare (e1,_)  (e2,_) = 
      let c1,c2 = cost e1,cost e2 in
      if c1 = c2 then 0
      else if c1 < c2 then -1
      else 1
  end

  module Heap = BatHeap.Make (OrderedType)

  (* Heap.t = work type = (hypothesis, free variable) *)
  type t = Heap.t * exp BatSet.t * string BatSet.t
  let empty = (Heap.empty, BatSet.empty, BatSet.empty)
 
  let print_set : t -> unit
  =fun (_,set,_) ->
    begin
      print_string " Processed forms : ";
      BatSet.iter (fun e -> print_endline (exp2str e ^ " ")) set;
      print_endline ""
    end

  let print_sset : t -> unit
  =fun (_,_,sset) ->
    begin
      print_string " Processed forms : ";
      BatSet.iter (fun e -> print_string (e ^ " ")) sset;
      print_endline ""
    end

  let explored : exp -> t -> bool
  =fun exp ((_,set,sset)) -> 
    let _ = Profiler.start_event "Worklist.explored" in
    let b1 = BatSet.mem (exp2str_mod_hole exp) sset in
(*    let b2 = BatSet.exists (eq_mod_hole exp) set in *)
    let _ = Profiler.finish_event "Worklist.explored" in
(*    (try 
    assert (b1 = b2)
    with _ -> print_endline (exp2str exp ^ " " ^ string_of_bool b1 ^ " " ^ string_of_bool b2); 
     print_set t;
     print_sset t;
    exit 0); *)
    b1

  let add : (exp -> int) -> work -> t -> t
  =fun cost (h,f) (lst,set,sset) ->
    let _ = Profiler.start_event "Worklist.add.exists" in
    let b_exist = explored h (lst,set,sset) in  
    let _ = Profiler.finish_event "Worklist.add.exists" in
    let res = 
      if b_exist then (lst,set,sset)
      else  
        let _ = Profiler.start_event "Worklist.add.insert" in
        let lst = Heap.add (h,f) lst in
        let _ = Profiler.finish_event "Worklist.add.insert" in
          (lst, BatSet.add h set, BatSet.add (exp2str_mod_hole h) sset) in
      res

  (* Worklist.choose *)    
  let choose : t -> (work * t) option
  =fun (lst,set,sset) ->
    try
    let h = Heap.find_min lst in
      Some (h, (Heap.del_min lst, set, sset))
    with _ -> None

  let print : t -> unit
  =fun (lst,set,_) -> ()
(*    print_endline "\n\n================ Worklist =================";
    List.iter (fun x -> 
      match x with
      | (e, Some f) -> print_endline (exp2str e ^ ", " ^ exp2str f) 
      | (e, None) -> print_endline (exp2str e ^ ", None")) lst;
    print_endline "===============================================\n\n"; flush stdout
*)
  let string_of_size : t -> string
  =fun (lst,set,sset) ->
    " " ^ string_of_int (Heap.size lst) ^ 
    " " ^ string_of_int (BatSet.cardinal set) ^ 
    " " ^ string_of_int (BatSet.cardinal sset)

  let size_of_list : t -> int
  =fun (lst,_,_) -> Heap.size lst

  let size_of_set : t -> int
  =fun (_,set,_) -> BatSet.cardinal set
end

let rec work : example list -> example list -> Worklist.t -> exp option
=fun pos_examples neg_examples worklist ->
  iter := !iter + 1;
  if !verbose >= 0 && !iter mod 1000 = 0 && not(!Options.simple)
  then begin
    let t = Sys.time () -. !t0 in
    let _ = t0 := Sys.time() in
    print_endline (string_of_int !iter ^ "." ^
       " Worklist size : " ^ Worklist.string_of_size worklist ^
       " took " ^ string_of_float t ^ "sec" ^ " total: " ^ string_of_float (Sys.time() -. t_total))
  end;
  match Worklist.choose worklist with (* choose minimal cost node *)
  | None -> None (* failed to discover a solution *)
  (* when e is a closed expression *)
  | Some ((e, None),worklist) ->
    let e = normalize e in 
    if !verbose >= 1 then print_endline ("Pick a closed expression: " ^ exp2str e);
    if consistent e pos_examples neg_examples 
    then
        Some e (* solution found *)
    else
         (work pos_examples neg_examples worklist)     
         
  (* when e is an expression with hole f *)
  | Some ((e, Some f),worklist) -> (* (work, t type) *)
    if !verbose >= 2 then print_endline (" search: " ^ 
            exp2str_w_outset e ^ " level: " ^string_of_int (level e));
    let b_hopeless = hopeless run e pos_examples neg_examples in
    if b_hopeless || needless e pos_examples neg_examples
    then work pos_examples neg_examples worklist
    else
      (* making new worklist *)
      work pos_examples neg_examples (
        List.fold_left (fun worklist e' -> 
        (* replace the hole inside e by e' *)
        let e_subst = subst e e' f in
        (* e_subst can have holes 
         because subst change just only one hole *)
        let e'' = normalize e_subst in
        let holes = holes e'' in
            match holes with
            | [] -> Worklist.add cost (e'', None) worklist
            | _ ->
              let f' = List.hd holes in
              Worklist.add cost (e'', Some f') worklist
      ) worklist (available()))

let perform : example list -> example list -> pgm option
=fun pos_examples neg_examples -> 
  let init_worklist = Worklist.add cost (init()) Worklist.empty in
    work pos_examples neg_examples init_worklist
