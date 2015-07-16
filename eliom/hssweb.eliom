{shared{
  open Eliom_lib
  open Eliom_content
  open Eliom_content.Html5.D.Raw
  open Eliom_parameter
  open Printf
}}


type t_param = { order:int; tmax:float; hmin:float; eps:float; hdump:float;
				 k:int; tsim:float; }

type t = { spec:string; param:t_param; yvar:int; fontsize:float; }

let record_of_pdata ((*_data,*)(spec,(order,(tmax,(hmin,(eps,(hdump,(k,(tsim,(yvar,fontsize)))))))))) = 
  let pvalue = {order=order;tmax=tmax;hmin=hmin;eps=eps;hdump=hdump;k=k;tsim=tsim;} in
  {spec=spec;param=pvalue;yvar=yvar;fontsize=fontsize;}

let default_pvalue spec = 
  let pvalue = {order=20; tmax=100.; hmin=1e-14; eps=1e-14; hdump=0.1; k=10000; tsim=10.;} in
  {spec=spec;param=pvalue;yvar=0;fontsize=1.;}


{server{
  open Format
  open Lexing
  open Hss

  module PPtree = Pretty.Make(Ptree)
  module PModel = Pretty.Make(Model)

  let report (b,e) =
    let l = b.pos_lnum in
    let fc = b.pos_cnum - b.pos_bol + 1 in
    let lc = e.pos_cnum - b.pos_bol + 1 in
      Format.fprintf str_formatter "line %d, characters %d-%d: " l fc lc

  (*let hss_process_body debug auto_length lb =*)
  let hss_process_body pvalue lb =
	(* kludge for the lazy evaluation *)
	let vars = ref [] in
	let dump = ref "" in
	let res = ref "" in

	(*flush_str_formatter ();*)

    begin try 
      let (ha,prop),params = Parser.main Lexer.token lb in
      let ha = Ptree.simplify ha in
      let ((_,vs,_,_) as ha),(aps,ap_locs,prop,len) = Model.make ha prop in
	  vars := vs;

      Simulating.step_max := pvalue.k;
      (*Simulating.time_max := if auto_length then 10. else 10.;*)
      Simulating.time_max := max pvalue.tsim len;

      Capd_sending.send_model ha aps;
	  let add_param k v params = 
		if Model_common.MParam.mem k params then params 
		else Model_common.MParam.add k v params in
	  let params = add_param "order" (float_of_int pvalue.order) params in
	  let params = add_param "t_max" pvalue.tmax params in
	  let params = add_param "h_min" pvalue.hmin params in
	  let params = add_param "epsilon" pvalue.eps params in
	  let params = add_param "dump_interval" pvalue.hdump params in
      Capd_sending.send_solving_params params;
      Capd_sending_stubs.set_debug (*debug*) false;

      let ap_fs = Simulating.simulate ha (aps,ap_locs) in
	  dump := Capd_simulating_stubs.get_dump_data ();

      let update_ap_fs (id,fs) =
          let fs = (Interval.zero, true)::fs in
          id, Some fs in
      let ap_fs = List.map update_ap_fs ap_fs in
      let ap_fs = Mitl_checking.mod_intervals false !Simulating.time_max ap_fs prop in
      begin match Mitl_checking.eval_at_zero ap_fs with
      | Some r -> Format.fprintf str_formatter "%b\n%!" r;
      | None   -> Format.fprintf str_formatter "unknown\n%!" end;

	  res := flush_str_formatter ();

	  (*vars (* return var list. *)*)
    with
    | Lexer.Lexical_error s -> 
	  report (lexeme_start_p lb, lexeme_end_p lb);
	  Format.fprintf str_formatter "lexical error: %s\n@." s;
	  res := flush_str_formatter ();
    | Parsing.Parse_error ->
	  let  loc = (lexeme_start_p lb, lexeme_end_p lb) in
	  report loc;
      Format.fprintf str_formatter "syntax error\n@.";
	  res := flush_str_formatter ();
    | Util.LError (e,l) -> 
	  report l; 
	  Format.fprintf str_formatter "lint error: %a\n@." Util.report e;
	  res := flush_str_formatter ();
    | Util.Error e -> 
	  Format.fprintf str_formatter "error: %a\n@." Util.report e;
	  dump := (Capd_simulating_stubs.get_dump_data ())^"{}]";
	  res := flush_str_formatter ();
	| _ ->
	  fprintf str_formatter "unexpected error\n@.%!";
	  res := flush_str_formatter ();
    end;

	(!vars,
	(*Capd_simulating_stubs.get_dump_data ()*) !dump,
	(*flush_str_formatter ()*) !res,
	!Simulating.time_max )


  (*let hss_process debug auto_length ha =*)
  let hss_process param ha =
    let lb = from_string ha in
	hss_process_body param lb

  let hss_process_file param filename =
    let cin = open_in filename in
    let lb = from_channel cin in
	hss_process_body param lb
}}


module Hssweb_app =
  Eliom_registration.App (
    struct
      let application_name = "hssweb"
    end)

let main_service =
  Eliom_service.Http.service ~path:[] ~get_params:Eliom_parameter.unit ()


let examples = [
  ("Arc", "arc.ha");
  ("Disk", "disk.ha");
  ("Rotation", "rotate.ha");
  ("Water tank", "watertank.ha");
  ("BB (simple)", "bb-simple.ha");
  ("BB (parabola)", "bb-parabola.ha");
  ("BB (movingtable)", "bb-movingtable.ha");
  ("Gas burner", "gasburner.ha");
  ("ATM 4", "atm4.ha");
]

let example_services =
  List.map
	(fun (nm,_fn) ->
	  Eliom_service.Http.service
	    ~path:["loadexample";nm]
	    ~get_params:unit
		() )
	examples

let example_list () =
  let elink ind (nm,_fn) = Html5.D.(li [a (List.nth example_services ind) [pcdata nm] ()]) in
  div ~a:[ a_id "example_list" ]
    [ div ~a:[ a_id "submenu_header" ] [pcdata "Examples:"];
	  ul ~a:[ a_id "submenu_body" ] (List.mapi elink examples);
	]


let submission_service = 
  Eliom_service.Http.post_service
	~fallback:main_service
    ~post_params:((*string "data" ** *)
				  string "spec" ** 
				  int "order" **
				  float "tmax" **
				  float "hmin" **
				  float "eps" **
				  float "hdump" **
				  int "k" **
				  float "tsim" **
				  int "yvar" **
				  float "fontsize" )
	()

let create_input_form vars v_dump v_res v_tsim v_spec pvalue v_yvar v_fontsize =
  fun ((*res,*) (spec, (order, (tmax, (hmin, (eps, (hdump, (k, (tsim, (yvar, fontsize)))))))))) ->
    let submenu = div ~a:[ a_id "submenu" ] 
	  [ example_list () ] in

	let spec_ta =
	  Html5.D.textarea ~a:[ a_id "spec"; 
							a_style ("font-size:"^(string_of_float v_fontsize)^"em;"); ] 
					   ~name:spec ~value:v_spec () in
    let left = div ~a:[ a_id "left" ] [ spec_ta ] in 

	let res_ta =
	  Html5.D.Raw.(textarea ~a:[ a_id "result";
								 a_style ("font-size:"^(string_of_float v_fontsize)^"em;"); ]
					   (pcdata v_res) ) in

	let create_input l input =
      div ~a:[ a_class ["param_input"] ] 
  	    [pcdata l; input] in
    let int_input l n v =
	  create_input l (Html5.D.int_input ~input_type:`Text ~name:n ~value:v ()) in
    let flt_input l n v =
	  create_input l (Html5.D.float_input ~input_type:`Text ~name:n ~value:v ()) in
	let yv_input =
	  match vars with
	  | [] ->
	    Html5.D.(int_select ~name:yvar (Option ([], 0, Some (pcdata ""), true)) [])
	  | v0::vs ->
	    Html5.D.(int_select ~name:yvar
	      (Option ([], 0, Some (pcdata v0), v_yvar=0))
	      (List.mapi (fun i l -> Option ([], (i+1), Some (pcdata l), v_yvar=(i+1))) vs) ) in
	let _ = {unit{
	  let open Lwt_js_events in
	  let inp = Html5.To_dom.of_select %yv_input in
	  async (fun () -> changes inp (fun _ _ ->
	    Js.Unsafe.fun_call (Js.Unsafe.variable "plot") 
		  [|Js.Unsafe.inject "plot"; Js.Unsafe.inject inp##value; 
			Js.Unsafe.eval_string %v_dump; Js.Unsafe.inject %v_tsim; |];
		Lwt.return ()
	  ))
	}} in
	let fs_input = 
	  let eq f1 f2 = abs_float (f1-.f2) < 1e-12 in
	  Html5.D.(float_select ~name:fontsize
		(Option ([], 0.6, Some (pcdata "0.6"), (eq v_fontsize 0.6)))
		(List.map (fun s -> Option ([], s, Some (pcdata (string_of_float s)), (eq v_fontsize s)))
		[1.;1.1;1.2;1.3;1.4] )) in
	let _ = {unit{
	  let open Lwt_js_events in
	  let inp = Html5.To_dom.of_select %fs_input in
	  let sta  = Html5.To_dom.of_textarea %spec_ta in
	  let rta  = Html5.To_dom.of_textarea %res_ta in
	  async (fun () -> changes inp (fun _ _ ->
	    let s = Js.to_string (inp##value) in
	    sta##style##fontSize <- Js.string (s^"em");
	    rta##style##fontSize <- Js.string (s^"em");
		Lwt.return ()
	  ))
	}} in
    let right = div ~a:[ a_id "right" ]
	  [ div ~a:[ a_id "ctrl" ] [
		  int_input "Order: " order pvalue.order;
		  flt_input "Tmax: "  tmax  pvalue.tmax;
		  flt_input "Hmin: "  hmin  pvalue.hmin;
		  flt_input "Eps: "   eps   pvalue.eps;
		  flt_input "Hdump: " hdump pvalue.hdump;
		  int_input "K: "     k     pvalue.k;
		  flt_input "Tsim: "  tsim  pvalue.tsim;
		  br ();
		  Html5.D.input ~input_type:`Submit ~value:"Run" ();
		  br ();
      	  div ~a:[ a_class ["param_input"] ] [ pcdata "Yvar: "; yv_input ];
      	  div ~a:[ a_class ["param_input"] ] [ pcdata "Fontsize: "; fs_input ];
		];
	    (*Html5.D.textarea ~a:[ a_id "data" ] ~name:data ~value:v_data ()*)
		res_ta;
	  ] in

    let input_pane = div ~a:[ a_id "input" ] 
	  [ left; right ] in

    Html5.D.([ submenu; input_pane ])


let gen_frontend phandler {spec=spec; param=pvalue; yvar=yvar; fontsize=fontsize;} =
  let vars, dump, res, tsim = if spec <> "" then 
	hss_process pvalue spec
  else [], "[]", "", 0. in

  let _ = {unit{
(*alert "%s" %spec;*)
    Js.Unsafe.fun_call (Js.Unsafe.variable "plot") 
	  [|Js.Unsafe.inject "plot"; Js.Unsafe.inject %yvar; Js.Unsafe.eval_string %dump;
		Js.Unsafe.inject %tsim; |];
  }} in

  let title_text = "HySIA (beta)" in
  let p_holder = div ~a:[ a_id "plot" ] [] in
  let iform = (Html5.D.post_form phandler 
	(create_input_form vars dump res tsim spec pvalue yvar fontsize) ()) in

  Lwt.return
	Html5.F.(html
		(head (title (pcdata title_text)) 
			[ css_link ~uri:(make_uri (Eliom_service.static_dir ()) ["css";"hssweb.css"]) ();
			  js_script ~uri:(make_uri (Eliom_service.static_dir ())
								["dygraph-combined.js"]) ();
			  js_script ~uri:(make_uri (Eliom_service.static_dir ())
								["plot.js"]) (); ])
		(body [ div ~a:[ a_id "pagebody" ] [ 
				div ~a:[ a_id "top" ] [
				  div ~a:[ a_id "title" ] [ h1 [pcdata title_text] ];
				  p_holder
				];
				iform;
				div ~a:[ a_id "bottom" ] 
				  [ pcdata "Copyright(C) 2015 "; 
					Html5.F.Raw.a
					  ~a:[a_href (Xml.uri_of_string "http://www.dsksh.com/")]
					  [pcdata "Daisuke Ishii"]; pcdata ". All rights reserved." ];
			  ]]))


let () =
  Hssweb_app.register
    ~service:submission_service
	(fun () pdata -> gen_frontend submission_service (record_of_pdata pdata));

  let register i (_,fn) =
	Hssweb_app.register
	  ~service:(List.nth example_services i)

	  (fun () () -> 
	    let ic = open_in ("static/ha/"^fn) in
	    try
	      let n = in_channel_length ic in
	      let s = String.create n in
	      really_input ic s 0 n;
	      close_in ic;
	
	      gen_frontend submission_service (default_pvalue s);
	    with e ->
	      close_in_noerr ic;
	      raise e )
  in
  let _ = List.mapi register examples in

  Hssweb_app.register
    ~service:main_service
	(fun () () -> gen_frontend submission_service (default_pvalue ""))
