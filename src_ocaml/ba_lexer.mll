{
  open Lexing
  open Ba_parser
    
  exception Lexical_error of string

  let id_or_keyword = 
    let h = Hashtbl.create 2 in
      List.iter (fun (s,k) -> Hashtbl.add h s k)
      ["define", DEFINE;
       "never", NEVER;
       "if", IF;
       "fi", FI;
       "goto", GOTO;
       "true", TRUE;
       "false", FALSE;
	  ];
    fun s -> 
	  try Hashtbl.find h s with Not_found -> ID s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; 
	    pos_bol = pos.pos_cnum + 1(*; pos_cnum=0*) }
}

let delim  = [' ' '\t' '\r']
let ws     = delim+
let digit  = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let ident  = letter (letter | digit | '_')*
let number = digit+

rule token = parse
  | '\n' 
      { newline lexbuf; token lexbuf }
  | ws
      { token lexbuf }
  | ident
      { id_or_keyword (Lexing.lexeme lexbuf) }

  | "/*"
      { (*comment_start_loc := loc lexbuf;*) comment lexbuf; token lexbuf }

  (*| "#" { lcomment lexbuf } (* TODO *)*)

  | "#"  { SHARP }

  | "("  { LP }
  | ")"  { RP }
  | "{"  { LB }
  | "}"  { RB }
  | ":"  { COL }
  | ";"  { SCOL }

  | "!"  { NEG }
  | "&"  { AND }
  | "|"  { OR }

  | "->" { ARROW }

  | eof   
      { EOF }
  | _ 
      { raise (Lexical_error ("illegal character: " ^ lexeme lexbuf)) }

(*and string buf = parse
  | '"'    { STRING (Buffer.contents buf) }
  | '\n'   { newline lexbuf; Buffer.add_char buf '\n' ; string buf lexbuf}
  | "\\\"" { Buffer.add_char buf '"'; string buf lexbuf }
  | _ as c { Buffer.add_char buf c; string buf lexbuf }
  | eof    { raise (Lexical_error ("unterminated string")) }
*)
and comment = parse
  | "*/"
      { () }
  | "/*"
      { comment lexbuf; comment lexbuf }
  | '\n'
      { newline lexbuf; comment lexbuf }
  | eof
      { raise (Lexical_error ("unterminated comment")) }
  | _
      { comment lexbuf }

and lcomment = parse
  | '\n' { newline lexbuf; token lexbuf }
  | _    { lcomment lexbuf }
  | eof  { EOF }
