open Format
open Lexing
open Hss

let usage = "usage: hss [options] input.txt"

let debug = ref false

let auto_length = ref false

let prop_file = ref None

let sprt = ref false
let bht  = ref false

let alpha = ref 0.01
let beta  = ref 0.01
let delta = ref 0.01
let theta = ref 0.9
let bht_thres = ref 100

let spec = [
  "-n",  Arg.Int (fun n -> Simulating.step_max := n), 
                                "sets the # steps to simulate";
  "-t",  Arg.Float (fun t -> Simulating.time_max := t), 
                                "sets the max simulation time";
  "-a",  Arg.Set auto_length,   "decide the simulation length automatically";
  "-g",  Arg.Set debug,         "sets the debug flag";
  
  "-p",  Arg.String (fun fn -> prop_file := Some fn),
                                "sets the property filename";

  "-sprt",      Arg.Set sprt,   "use SPRT";
  "-bht",       Arg.Set bht,    "use BHT";
  "-alpha",     Arg.Float (fun v -> alpha := v), 
                                "sets the value alpha";
  "-beta",      Arg.Float (fun v -> beta  := v),
                                "sets the value beta";
  "-delta",     Arg.Float (fun v -> delta := v),
                                "sets the value delta";
  "-theta",     Arg.Float (fun v -> theta := v),
                                "sets the value theta";
  "-bht_thres", Arg.Int (fun v -> bht_thres := v),
                                "sets the threshold for BHT";
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


(*let process buff =
  try 
    let (ha,_prop),_params = Parser.main Lexer.token buff in
    let ha = Ptree.simplify ha in
      fprintf str_formatter "@[%a@]@." PPtree.print_ha ha;
      flush_str_formatter
  with
    | Lexer.Lexical_error s -> 
	  report (lexeme_start_p buff, lexeme_end_p buff);
	  printf "lexical error: %s\n@." s;
 	  exit 1
    | Parsing.Parse_error ->
	  let  loc = (lexeme_start_p buff, lexeme_end_p buff) in
	  report loc;
      printf "syntax error\n@.";
	  exit 1
    | Util.LError (e,l) -> 
	  report l; 
	  printf "lint error: %a\n@." Util.report e;
	  exit 1
    | Util.Error e -> 
	  printf "error: %a\n@." Util.report e;
	  exit 1
    | _ ->
      printf "unexpected error\n@.";
      exit 1
*)

let proc_sprt ha (aps,ap_locs) prop =

    let bnd_a = (1. -. !beta) /. !alpha in
    let bnd_b = !beta /. (1. -. !alpha) in

    let p0 = !theta +. !delta in
    let p1 = !theta -. !delta in

    let n = ref 0 in
    let x = ref 0 in
    let x_ = ref 0 in
    let p0n = ref 1. in
    let p1n = ref 1. in
    while !p1n /. !p0n < bnd_a && !p1n /. !p0n > bnd_b do
        printf "%f < %f < %f\n@." bnd_b (!p1n /. !p0n) bnd_a;

        incr n;
        let ap_fs = 
        try
            Some (Simulating.simulate ha (aps,ap_locs))
        with
        | Util.Error e -> 
    	  printf "error: %a\n@." Util.report e;
          None;
        | _ ->
          printf "unexpected error\n@.";
          None;
        in
        match ap_fs with
        | Some ap_fs -> begin
            (*List.map (fun (apid,fs) -> Printf.printf "AP%d: %d\n%!" apid (List.length fs)) ap_fs;*)
            let update_ap_fs (id,fs) =
                let fs = (Interval.zero, true)::fs in
                id, Some fs in
            let ap_fs = List.map update_ap_fs ap_fs in
            let ap_fs = Mitl_checking.propagate !debug ap_fs prop in
            (*print_endline "check done";*)
            (*printf "%a" Mitl_checking.print_fs ap_fs;*)
            match Mitl_checking.eval_at_zero ap_fs with
            | Some res -> begin Printf.printf "%b\n%!" res;
                if res 
                then (incr x;  p0n := !p0n *. p0; p1n := !p1n *. p1)
                else (incr x_; p0n := !p0n *. (1.-.p0); p1n := !p1n *. (1.-.p1)) end
            | None     -> begin Printf.printf "unknown\n%!";
                (p0n := !p0n *. (1.-.p0); p1n := !p1n *. p1) end
        end;
        | None -> 
            (p0n := !p0n *. (1.-.p0); p1n := !p1n *. p1)
            (*(p0n := !p0n *. p0; p1n := !p1n *. (1.-.p1))*)
    done;
    if !p1n /. !p0n <= bnd_b 
    then Printf.printf "H0 is accepted\n%!"
    else Printf.printf "H1 is accepted\n%!";
    Printf.printf "n: %d, x: %d, x': %d\n%!" !n !x !x_


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
    printf "@[prop: %a@]\n@." Ptree.print_prop prop;

    let ha,(aps,ap_locs,prop,len) = Model.make ha prop in
    if !debug then begin
      printf "@[%a@]@." PModel.print_ha ha;

      let pp i (hash,ap) = 
          printf "@[%d(%d): %a@." i hash Model.print_dual ap
      in
      let _ = Util.mapi pp aps in
      printf "length: %f@]@." len
    end;

    if !auto_length then Simulating.time_max := len;

    Capd_sending.send_model ha aps;
    Capd_sending.send_solving_params params;
    Capd_sending_stubs.set_debug !debug;

    if !sprt then proc_sprt ha (aps,ap_locs) prop
    else
        let ap_fs = Simulating.simulate ha (aps,ap_locs) in
            (*List.map (fun (apid,fs) -> Printf.printf "AP%d: %d\n%!" apid (List.length fs)) ap_fs;*)
        let update_ap_fs (id,fs) =
            let fs = (Interval.zero, true)::fs in
            id, Some fs in
        let ap_fs = List.map update_ap_fs ap_fs in
        let ap_fs = Mitl_checking.propagate !debug ap_fs prop in
        (*print_endline "check done";*)
        (*printf "%a" Mitl_checking.print_fs ap_fs;*)
        match Mitl_checking.eval_at_zero ap_fs with
        | Some res -> Printf.printf "%b\n%!" res
        | None     -> Printf.printf "unknown\n%!";
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
