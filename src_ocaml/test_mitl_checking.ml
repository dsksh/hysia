open Format
open Lexing
open OUnit2
open Hss.Interval
open Hss

let verify debug env str =
  let prop = Parser.property Lexer.token (from_string str) in
  if debug then
    printf "@[prop(Ptree): %a@]\n@." Ptree.print_prop prop;

  let vs,fs = List.split env in
(*List.map (fun v -> Printf.printf " %s" v) vs;
print_endline "";*)
  let aps,_,prop,_ = Model.make_prop vs prop in
  if debug then
    printf "@[prop(Model): %a@]\n@." Model.print_prop prop;
(*List.map (fun (i,d) -> Printf.printf " %d %d" i d.Hashcons.tag) aps;
print_endline "";
if fs <> [] then begin
  let print = function
      | Some is -> 
              List.map (fun i -> Printf.printf " %f %f" (fst i).inf (fst i).sup) is;
              ()
      | None ->
              printf " none";
  in
  List.map print fs;
  print_endline "";
end;*)

  let ap_fs = List.combine aps fs in
  let ap_fs = List.map (fun ((apid,_),fs) -> apid, fs) ap_fs in

  try
    let ap_fs = Mitl_checking.propagate debug ap_fs prop in
    let res = Mitl_checking.eval_at_zero ap_fs in
    if debug then begin
        match res with
        | Some res -> printf "@[  result: %b@]\n@." res
        | None     -> printf "@[  result: unknown@]\n@.";
    end;
    res
  with
  | Util.Error _e -> 
          if debug then printf "@[  result: unknown@]\n@.";
          None

(*let () =
  try 
    verify [] "prop true";
    verify [] "prop false";
    verify [("x",[])] "prop x";
    verify [("x",[])] "prop G[0,10] x";
    verify [("x",[({inf=1.;sup=2.},false)])] "prop G[0,10] x";
    verify [("x",[({inf=5.;sup=5.},false)])] "prop G[0,4] x";
    verify [("x",[({inf=1.;sup=2.},false)]);
            ("y",[({inf=5.;sup=5.},false)]) ]
           "prop G[0,10] x & G[0,4] y";

  let prop = Parser.property Lexer.token (from_string "prop G[0,10] x") in
  printf "@[prop: %a@]\n@." Ptree.print_prop prop;

  let aps,_,prop,len = Model.make_prop ["x"] prop in

  let ap_fs = List.map (fun (apid,_) -> apid, Some [({inf=1.;sup=2.},false)]) aps in

  let ap_fs = Mitl_checking.check true len ap_fs prop in
  match Mitl_checking.eval_at_zero ap_fs with
  | Some res -> printf "@[result: %b@]\n@." res
  | None     -> printf "@[result: unknown@]\n@."

  with
    | Lexer.Lexical_error s -> 
	  (*report (lexeme_start_p lb, lexeme_end_p lb);*)
	  printf "lexical error: %s\n@." s;
 	  exit 1
    | Parsing.Parse_error ->
	  (*let  loc = (lexeme_start_p lb, lexeme_end_p lb) in
	  report loc;*)
      printf "syntax error\n@.";
	  exit 1
    | Util.LError (e,l) -> 
	  (*report l; *)
	  printf "lint error: %a\n@." Util.report e;
	  exit 1
    | Util.Error e -> 
	  printf "error: %a\n@." Util.report e;
    | _ ->
      printf "unexpected error\n@.";
      exit 1
*)

let test_simple _ =
    assert_equal (Some true)  (verify false [] "prop true");
    assert_equal (Some false) (verify false [] "prop false");
    assert_equal (Some false) (verify false [] "prop !true");
    assert_equal (Some true)  (verify false [] "prop !false");
    assert_equal (Some true)  (verify false [] "prop true /\\ true");
    assert_equal (Some true)  (verify false [] "prop true \\/ true");
    ()

let test2 _ =
    assert_equal (Some true)  (verify false [("x",None)] "prop x");
    assert_equal (Some true)  (verify false [("x",None)] "prop G[0,10] x");
    assert_equal (Some false)  
        (verify false [("x",Some [({inf=0.;sup=0.},true); ({inf=1.;sup=2.},false)])] "prop G[0,5] x");
    assert_equal (Some true)  
        (verify false [("x",Some [({inf=0.;sup=0.},true); ({inf=5.;sup=5.},false)])] "prop G[0,4] x");
    assert_equal (Some false)  
        (verify false [("x",Some [({inf=0.;sup=0.},true); ({inf=5.;sup=5.},false)])] "prop G[0,5] x");
    assert_equal (Some true) (verify false
            [("x", None);
             ("y", Some [({inf=0.;sup=0.},true); ({inf=5.;sup=5.},false)]) ]
            "prop G[0,10] x & G[0,4] y");
    assert_equal (Some false) (verify false
            [("x", Some [({inf=0.;sup=0.},true); ({inf=1.;sup=2.},false)]);
             ("y", Some [({inf=0.;sup=0.},true); ({inf=5.;sup=5.},false)]) ]
            "prop G[0,10] x & G[0,4] y");
    ()

let test3 _ =
    assert_equal (Some true)  (verify false
            [("x", Some [({inf=0.;sup=0.},true); ({inf=1.;sup=2.},false)]);
             ("y", Some [({inf=0.;sup=0.},true); ({inf=1.5;sup=3.},false)]) ]
            "prop (x & y)");
    assert_equal (Some true)  (verify false
            [("x", Some [({inf=0.;sup=0.},true)]);
             ("y", Some [({inf=(-1.);sup=0.},true)]) ]
            "prop (x & y)");
    (*assert_raises (Error UnknownOverlap) (fun _ -> *)
    assert_equal None
        (verify false
            [("x", Some [({inf=0.;sup=0.},true)]);
             (*("x", None);*)
             ("y", Some [({inf=(-1.);sup=1.},true)]) ]
            "prop (x & y)");
    assert_equal (Some true) (verify false
            [("x", Some [({inf=0.;sup=0.},true)]);
             ("y", Some [({inf=(-1.);sup=0.},true); ({inf=1.;sup=1.},false)])  ]
            "prop (x & y)");
    assert_equal (Some true) (verify false
            [("x", Some [({inf=0.;sup=0.},true); ({inf=2.;sup=2.},false)]);
             ("y", Some [({inf=0.;sup=0.},true); ({inf=1.;sup=1.},false)])  ]
            "prop (x & y)");
    assert_equal (Some true) (verify false
            [("x", Some [({inf=1.;sup=1.},true); ({inf=3.;sup=3.},false)]);
             ("y", Some [({inf=1.;sup=1.},true); ({inf=2.;sup=2.},false)])  ]
            "prop F[1,1] (x & y)");
    assert_equal (Some true) (verify false
            [("x", Some [({inf=0.;sup=0.},true); ({inf=1.;sup=1.},false)]);
             ("y", Some [({inf=0.;sup=0.},true); ({inf=2.;sup=2.},false)]);
             ("z", Some [({inf=0.;sup=0.},true); ({inf=3.;sup=3.},false)])  ]
            "prop (x & y & z)");
    ()

let test4 _ =
    assert_equal (Some true)  (verify false
            [("x", Some [({inf=0.5;sup=0.5},true); ({inf=1.;sup=1.},false); 
                         ({inf=1.5;sup=1.5},true); ({inf=2.;sup=2.},false) ])]
            "prop (true U[0,3.5] x)");
    assert_equal (Some true)  (verify false
            [("x", Some [({inf=0.5;sup=0.5},true); ({inf=1.;sup=1.},false); 
                         ({inf=1.5;sup=1.5},true); ({inf=2.;sup=2.},false); 
                         ({inf=2.5;sup=2.5},true); ({inf=3.;sup=3.},false) ])]
            "prop (true U[0,3.5] x)");
    ()

let suite = "Test MITL verification" >:::
    ["test 1" >:: test_simple;
     "test 2" >:: test2;
     "test 3" >:: test3;
     "test 4" >:: test4;
    ]

let _ =
    run_test_tt_main suite
