open Vocab
open Options
open Lang
open Synthesizer

let usageMsg = "Usage: main.native -input filename"

(* take a onestep for x *)
let onestep_xto01 : string -> string list
= fun str ->
  if not (String.contains str 'X') 
  then [str]
  else
    let idx = String.index str 'X' in
    let str1 = str in
    let str2 = Bytes.copy str in
    let _ = Bytes.set str1 idx '0' in
    let _ = Bytes.set str2 idx '1' in
    [str1]@[str2]

(* onestep for all strings *)
let onestep_strlst : string list -> string list 
= fun strlst -> 
  List.fold_left (fun acc str -> acc@(onestep_xto01 str)) [] strlst 

let read_examples : string -> (str list*str list) option
= fun filename -> 
  if filename = "" then (print_endline usageMsg; exit 0)
  else
    let lines : string list = BatEnum.fold (fun a line -> a@[line]) [] (BatFile.lines_of filename) in
    let _ = mode := if List.exists (fun str -> String.contains str 'X') lines then IDIOM
                    else DEFAULT in
    let lines = if !mode = IDIOM then fix onestep_strlst lines
    else lines in
    (* find the position of positive examples *)
    let (p_pos,_) = List.fold_left (fun (pos,idx) l ->
                        if BatString.get l 0 = '+' then (idx,idx+1) else (pos,idx+1)) (0,0) lines in
    (* find the position of negative examples *)
    let (n_pos,idx) = List.fold_left (fun (pos,idx) l -> 
                        if BatString.get l 0 = '-' then (idx,idx+1) else (pos,idx+1)) (0,0) lines in
    let pos_examples = list_get lines (p_pos+1) (n_pos-1) in
    let neg_examples = list_get lines (n_pos+1) (List.length lines - 1) in
    let conflicting = List.exists (fun p -> List.mem p neg_examples) pos_examples in
    if conflicting 
      then None
    else 
      let trans line = BatString.fold_right (fun c l -> (char2alpha c)::l) line [] in
        Some (List.map trans pos_examples, 
              List.map trans neg_examples)

let sanity_check exp pos_examples neg_examples = 
  let res2str b = match b with true -> "accept" | false -> "reject" in
  List.iter (fun example ->
    print_endline (" - " ^ exp2str exp ^ "  " ^ str2str example ^ 
                   " : " ^ res2str (run exp example))
  ) pos_examples;
  List.iter (fun example ->
    print_endline (" - " ^ exp2str exp ^ "  " ^ str2str example ^ 
                   " : " ^ res2str (run exp example))
  ) neg_examples


let main () =
 let _ = Arg.parse options (fun s -> ()) usageMsg in
 let t0 = Sys.time () in
 let _ = Profiler.start_logger () in
 let examples = read_examples !inputfile in
 match examples with
 | None -> print_endline "No solution exists"  
 | Some (pos_examples, neg_examples) ->
   if not !Options.simple 
   then
     let _ = 
       print_endline "============  INPUT  =============";
       print_endline "- Positive Examples:";
       print_examples pos_examples;
       print_endline "- Negative Examples:";
       print_examples neg_examples;
       print_endline "==========  PROCESSING  ==========" in
     let pgm = Synthesizer.perform pos_examples neg_examples in
     let t1 = Sys.time () in
       print_endline "============  OUTPUT  ============";
     (match pgm with
      | None -> print_endline "Fail to synthesize"
      | Some pgm -> 
        let _ = pp pgm in
          print_endline ("Level: " ^ string_of_int (level pgm));
          sanity_check pgm pos_examples neg_examples;
          print_endline "============  REPORT  =============";
          print_endline ("Iter : " ^ string_of_int !iter);
          print_endline ("Time : " ^ string_of_float (t1 -. t0) ^ " seconds\n");
          Profiler.report stdout)
   else
     let pgm = Synthesizer.perform pos_examples neg_examples in
     (match pgm with
      | None -> print_endline "Fail to synthesize"
      | Some pgm -> pp pgm)
         
let _ = main () 
