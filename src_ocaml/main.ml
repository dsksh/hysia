open Format
open Lexing
open Hss

let usage = "usage: hss [options] input.txt"

let debug = ref false

let prop_file = ref None

let spec = [
  "-n",  Arg.Int (fun n -> Simulating.step_max := n), 
                                "sets the # steps to simulate";
  "-g",  Arg.Set debug,         "sets the debug flag";
  
  "-p",  Arg.String (fun fn -> prop_file := Some fn),
                                "sets the property filename";
]

let file = ref "stdin"
let cin =
  let ofile = ref None in
  Arg.parse spec (fun s -> ofile := Some s) usage;
  match !ofile with
    | Some f -> file := f ; open_in f 
    | None -> stdin

let reset_lexbuf lexbuf = 
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- {
    pos with pos_lnum = 1; pos_bol = pos.pos_cnum;
  }

let report (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
    printf "File \"%s\", line %d, characters %d-%d: " !file l fc lc


module PPtree = Pretty.Make(Ptree)
(*module PBa_ptree = Pretty.Make(Ba_ptree)*)
module PModel = Pretty.Make(Model)


let () =
  let lb = from_channel cin in 
  try 
    let ptree,params = Parser.main Lexer.token lb in
    close_in cin;
    let ptree = Ptree.simplify ptree in
    (*if !debug then
      printf "@[%a@]@." PPtree.print ptree;*)

    let model = Model.make ptree in
    if !debug then
      printf "@[%a@]@." PModel.print model;

    (*(* parse MTL property file *)
    match !prop_file with
    | Some f -> 
        file := f;
        let cin = open_in f in
        let lb = from_channel cin in
        begin try
          print_endline "load prop file";
          Lexing.flush_input lb;
          reset_lexbuf lb;
          let locs = Ba_parser.main Ba_lexer.token lb in
          close_in cin;
          let ptree = [],[],(),locs in
          printf "@[%a@]@." PBa_ptree.print ptree
        with
        | Ba_lexer.Lexical_error s -> 
    	  report (lexeme_start_p lb, lexeme_end_p lb);
    	  printf "nc lexical error: %s\n@." s;
    	  exit 1
        | Parsing.Parse_error ->
    	  let  loc = (lexeme_start_p lb, lexeme_end_p lb) in
    	  report loc;
          printf "syntax error\n@.";
    	  exit 1
        end
    | None -> print_endline "skip loading"; ();
    *)

    Capd_sending.send_model model;
    Capd_sending.send_solving_params params;
    Capd_sending_stubs.set_debug !debug;
    Simulating.simulate model;
    ()
  with
    | Lexer.Lexical_error s -> 
	  report (lexeme_start_p lb, lexeme_end_p lb);
	  printf "lexical error: %s\n@." s;
 	  exit 1
    | Parsing.Parse_error ->
	  let  loc = (lexeme_start_p lb, lexeme_end_p lb) in
	  report loc;
      printf "syntax error\n@.";
	  exit 1
    | Util.LError (e,l) -> 
	  report l; 
	  printf "lint error: %a\n@." Util.report e;
	  exit 1
    | Util.Error e -> 
	  printf "error: %a\n@." Util.report e;
    | _ ->
      printf "unexpected error\n@.";
      exit 1;
