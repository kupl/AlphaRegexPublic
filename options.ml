let inputfile = ref ""
let verbose = ref 0
let profile = ref false
let simple = ref false

let options = 
  [
    ("-v", (Arg.Int (fun n -> verbose := n)), "verbose mode: 0, 1, 2");
    ("-input", (Arg.String (fun s -> inputfile := s)), "input file containing positive and negative examples");
    ("-profile", (Arg.Set profile), "profiling");
    ("-simple", (Arg.Set simple), "simple")
  ]
