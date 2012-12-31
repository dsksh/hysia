open Format
open Lexing
open Hss
open Model_common

let usage = "usage: hss [options] input.txt"

let debug = ref false

let spec = [
  "-g",  Arg.Set debug,         "sets the debug flag";
]

let file = ref "stdin"
let cin =
  let ofile = ref None in
    Arg.parse spec (fun s -> ofile := Some s) usage;
    match !ofile with
      | Some f -> file := f ; open_in f 
      | None -> stdin

let report (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
    printf "File \"%s\", line %d, characters %d-%d:" !file l fc lc


let integrate args =
  let a1 = try MParam.find "t_end" args with Not_found -> 1. in
  let a2 = try MParam.find "order" args with Not_found -> 10. in
  let a3 = try MParam.find "h_min" args with Not_found -> 0.1 in
  let a4 = try MParam.find "h_max" args with Not_found -> 1. in
  Capd_stubs.integrate a1 a2 a3 a4


module PPtree = Pretty.Make(Ptree)
module PModel = Pretty.Make(Model)


let () =
(*  Capd_stubs.test1 "var:t,x,v; fun:1,v,-sin(x);"*)

  let lb = from_channel cin in 
  try 
    let ptree,args = Parser.main Lexer.token lb in
    let ptree = Ptree.simplify ptree in
    printf "@[%a@]@." PPtree.print ptree;

    let model = Model.make ptree in
    printf "@[%a@]@." PModel.print model;

    Capd_sending.send_model model;
    integrate args;
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
    | Util.Error(e,l) -> 
	report l; 
	printf "lint error: %a\n@." Util.report e;
	exit 1
    | _ ->
        printf "unexpected error\n@.";
	exit 1
