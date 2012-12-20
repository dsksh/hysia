open Format
open Lexing
open Hss

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


let () =
(*  Capd_stubs.test1 "var:t,x,v; fun:1,v,-sin(x);"*)

  let lb = from_channel cin in 
  try 
    let ptree = Parser.main Lexer.token lb in
      printf "@[%a@]@." Pretty.print_ptree ptree;
      Capd_stubs.init 3;
      Capd_sending.send_ptree ptree;
      Capd_stubs.integrate ();
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
    | _ ->
        printf "unexpected error\n@.";
	exit 1
