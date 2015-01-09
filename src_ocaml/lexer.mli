exception Lexical_error of string
val id_or_keyword : string -> Parser.token
val newline : Lexing.lexbuf -> unit
val __ocaml_lex_tables : Lexing.lex_tables
val token : Lexing.lexbuf -> Parser.token
val __ocaml_lex_token_rec : Lexing.lexbuf -> int -> Parser.token
val string : Buffer.t -> Lexing.lexbuf -> Parser.token
val __ocaml_lex_string_rec : Buffer.t -> Lexing.lexbuf -> int -> Parser.token
val comment : Lexing.lexbuf -> unit
val __ocaml_lex_comment_rec : Lexing.lexbuf -> int -> unit
val lcomment : Lexing.lexbuf -> Parser.token
val __ocaml_lex_lcomment_rec : Lexing.lexbuf -> int -> Parser.token
