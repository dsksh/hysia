open Format
open Lexing
open Hss

let usage = "usage: hss [options] input.txt"

let debug = ref false

let prop_file = ref None

let spec = [
  "-n",  Arg.Int (fun n -> Simulating.step_max := n), 
                                "sets the # steps to simulate";
  "-t",  Arg.Float (fun t -> Simulating.time_max := t), 
                                "sets the max simulation time";
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


let _ = Random.self_init ()


module PPtree = Pretty.Make(Ptree)
(*module PBa_ptree = Pretty.Make(Ba_ptree)*)
module PModel = Pretty.Make(Model)


let () =
  let lb = from_channel cin in 
  try 
    let (ha,prop),params = Parser.main Lexer.token lb in
    close_in cin;
    let ha = Ptree.simplify ha in
    (*if !debug then begin
      printf "@[%a@]@." PPtree.print_ha ha;
      printf "@[prop: %a@]\n@." Ptree.print_prop prop
    end *)

    let ha,prop = Model.make ha prop in
    if !debug then begin
      printf "@[%a@]@." PModel.print_ha ha;

      let aps,_ = prop in
      let pp i (hash,ap) = 
          printf "@[%d(%d): %a@]@." i hash Model.print_dual ap
      in
      (*Model.APMap.iter pp aps;*)
      Util.mapi pp aps; ()
    end;

    Capd_sending.send_model ha prop;
    Capd_sending.send_solving_params params;
    Capd_sending_stubs.set_debug !debug;
    let ap_fs = Simulating.simulate ha prop in
    List.map (fun (apid,fs) -> Printf.printf "AP%d: %d\n%!" apid (List.length fs)) ap_fs;
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
