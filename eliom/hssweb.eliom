{shared{
  open Eliom_lib
  open Eliom_content
  open Eliom_content.Html5.D.Raw
  open Eliom_parameter
  open Printf
}}


type t_param = { order:int; tmax:float; hmin:float; eps:float; hdump:float;
				 k:int; tsim:float; }

type t = { spec:string; param:t_param; yvar:int; }

let record_of_pdata (_data,(spec,(order,(tmax,(hmin,(eps,(hdump,(k,(tsim,yvar))))))))) = 
  let pvalue = {order=order;tmax=tmax;hmin=hmin;eps=eps;hdump=hdump;k=k;tsim=tsim;} in
  {spec=spec;param=pvalue;yvar=yvar;}


{server{
  open Format
  open Lexing
  open Hss

  module PPtree = Pretty.Make(Ptree)
  module PModel = Pretty.Make(Model)

  (*let hss_process_body debug auto_length lb =*)
  let hss_process_body pvalue lb =
    (begin try 
      let (ha,prop),params = Parser.main Lexer.token lb in
      let ha = Ptree.simplify ha in
      let ((_,vars,_,_) as ha),(aps,ap_locs,prop,len) = Model.make ha prop in

      Simulating.step_max := pvalue.k;
      (*Simulating.time_max := if auto_length then 10. else 10.;*)
      Simulating.time_max := pvalue.tsim;

      Capd_sending.send_model ha aps;
	  let params = Model_common.MParam.add "order" (float_of_int pvalue.order) params in
	  let params = Model_common.MParam.add "t_max" pvalue.tmax params in
	  let params = Model_common.MParam.add "h_min" pvalue.hmin params in
	  let params = Model_common.MParam.add "epsilon" pvalue.eps params in
	  let params = Model_common.MParam.add "dump_interval" pvalue.hdump params in
      Capd_sending.send_solving_params params;
      Capd_sending_stubs.set_debug (*debug*) false;

      let ap_fs = Simulating.simulate ha (aps,ap_locs) in
      let update_ap_fs (id,fs) =
          id, Some fs in
      let ap_fs = List.map update_ap_fs ap_fs in
      let ap_fs = Mitl_checking.check false !Simulating.time_max ap_fs prop in
      begin match Mitl_checking.eval_at_zero ap_fs with
      | Some res -> fprintf str_formatter "%b\n%!" res
      | None     -> fprintf str_formatter "unknown\n%!" end;

	  vars (* return var list. *)
    with
      | _ ->
        fprintf str_formatter "unexpected error\n@.";
		[]
    end,

	Capd_simulating_stubs.get_dump_data () )


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
  Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

let load_example_service =
  Eliom_service.service
    ~path:["loadexample"]
    ~get_params:Eliom_parameter.(string "filename")
	()


let get_file =
  Eliom_registration.File.register_service
    ~path:["getfile"]
    ~get_params:unit
    (fun () () -> Lwt.return "../examples/arc.ha")


let example_list () =
  let elink name fn = Html5.D.a load_example_service [pcdata name] (fn) in
  div ~a:[ a_id "example_list" ]
    [ div ~a:[ a_id "submenu_header" ] [pcdata "Examples:"];
	  ul ~a:[ a_id "submenu_body" ]
	  [ li [elink "Arc" "arc.ha"];
	  	li [elink "Disk" "disk.ha"];
	  	li [elink "Rotate" "rotate.ha"];
	  	li [elink "BB (simple)" "bb-simple.ha"];
	  	li [elink "BB (parabola)" "bb-parabola.ha"];
	  ]]

let submission_service = 
  Eliom_service.post_service
	~fallback:main_service
    ~post_params:(string "data" ** 
				  string "spec" ** 
				  int "order" **
				  float "tmax" **
				  float "hmin" **
				  float "eps" **
				  float "hdump" **
				  int "k" **
				  float "tsim" **
				  int "yvar" )
	()

let create_input_form vars v_data v_spec pvalue v_yvar =
  fun (data, (spec, (order, (tmax, (hmin, (eps, (hdump, (k, (tsim, yvar))))))))) ->
    let submenu = div ~a:[ a_id "submenu" ] 
	  [ example_list () ] in

    let left = div ~a:[ a_id "left" ] 
	  [ Html5.D.textarea ~a:[ a_id "spec"; a_style "font-size:1em;" ] ~name:spec ~value:v_spec () ] in

	let create_input l input =
      div ~a:[ a_class ["param_input"] ] 
  	    [pcdata l; input] in
    let int_input l n v =
	  create_input l (Html5.D.int_input ~input_type:`Text ~name:n ~value:v ()) in
    let flt_input l n v =
	  create_input l (Html5.D.float_input ~input_type:`Text ~name:n ~value:v ()) in
    let right = div ~a:[ a_id "right" ]
	  [ div ~a:[ a_id "ctrl" ] [
		  int_input "Order: " order pvalue.order;
		  flt_input "Tmax: "  tmax  pvalue.tmax;
		  flt_input "Hmin: "  hmin  pvalue.hmin;
		  flt_input "Eps: "   eps   pvalue.eps;
		  flt_input "Hdump: " hdump pvalue.hdump;
		  int_input "K: "     k     pvalue.k;
		  flt_input "Tsim: "  tsim  pvalue.tsim;
      	  div ~a:[ a_class ["param_input"] ] 
  	        [pcdata "Yvar: "; 
			 match vars with
			 | [] ->
			   Html5.D.(int_select ~name:yvar (Option ([], 0, Some (pcdata ""), true)) [])
			 | v0::vs ->
			   Html5.D.(int_select ~name:yvar
			     (Option ([], 0, Some (pcdata v0), v_yvar=0))
			     (List.mapi (fun i l -> Option ([], (i+1), Some (pcdata l), v_yvar=(i+1))) vs) );
			];
		  Html5.D.input ~input_type:`Submit ~value:"Run" () 
		];
	    Html5.D.textarea ~a:[ a_id "data" ] ~name:data ~value:v_data ()
	  ] in

    let input_pane = div ~a:[ a_id "input" ] 
	  [ left; right ] in

    Html5.D.([ submenu; input_pane ])


let gen_frontend phandler {spec=spec; param=pvalue; yvar=v_yvar;} =
  let vars, res = if spec <> "" then 
	hss_process pvalue spec
  else [], "[]" in

  let _ = {unit{
	(*Eliom_lib.alert "%d\n" (List.length %vars);*)
    Js.Unsafe.fun_call (Js.Unsafe.variable "plot") 
	  [|Js.Unsafe.inject "plot"; Js.Unsafe.inject %v_yvar; Js.Unsafe.eval_string %res|]
  }} in

  let title_text = "Validated simulator for hybrid automata" in
  let p_holder = div ~a:[ a_id "plot" ] [] in
  let iform = (Html5.D.post_form phandler 
	(create_input_form vars res spec pvalue v_yvar) ()) in

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
			  ]]))


(*let process_service = 
  Eliom_service.post_service
	~fallback:main_service
    ~post_params:(string "ha")
	()
let process =
  Eliom_registration.String.register
	~service:process_service
    (fun () (ha) -> 
	  let r = hss_process false true ha in
	  Lwt.return (r, "application/json"))
*)

(*let default_option = (20, (100., (1e-14, (1e-14, (0.1, (10000, (10., 0)))))))*)
let default_pvalue = {order=20; tmax=100.; hmin=1e-14; eps=1e-14; hdump=0.1; k=10000; tsim=10.;}

let () =
  Hssweb_app.register
    ~service:submission_service
	(*(fun () (data,(spec,(order,(tmax,(hmin,(eps,(hdump,(k,(tsim,yvar)))))))) -> 
	  gen_frontend submission_service (spec,(order,(tmax,(hmin,(eps,(hdump,(k,(tsim,yvar))))))));*)
	(fun () pdata -> gen_frontend submission_service (record_of_pdata pdata));

  Hssweb_app.register
	~service:load_example_service

    (fun (fn) () -> 
      let ic = open_in ("../examples/"^fn) in
      try
        let n = in_channel_length ic in
        let s = String.create n in
        really_input ic s 0 n;
        close_in ic;

	    gen_frontend submission_service {spec=s; param=default_pvalue; yvar=0;};
      with e ->
        close_in_noerr ic;
        raise e);

  Hssweb_app.register
    ~service:main_service
	(fun () () -> gen_frontend submission_service {spec=""; param=default_pvalue; yvar=0;})
