{shared{
  open Eliom_lib
  open Eliom_content
  open Eliom_content.Html5.D.Raw
  open Eliom_parameter
  open Printf
}}

{server{
  open Format
  open Lexing
  open Hss

  module PPtree = Pretty.Make(Ptree)
  module PModel = Pretty.Make(Model)

  let hss_process_body debug auto_length lb =
    begin try 
      let (ha,prop),params = Parser.main Lexer.token lb in
      let ha = Ptree.simplify ha in
      let ha,(aps,ap_locs,prop,len) = Model.make ha prop in

      Simulating.time_max := if auto_length then len else 10.;

      Capd_sending.send_model ha aps;
      Capd_sending.send_solving_params params;
      Capd_sending_stubs.set_debug debug;

      let ap_fs = Simulating.simulate ha (aps,ap_locs) in
      let update_ap_fs (id,fs) =
          id, Some fs in
      let ap_fs = List.map update_ap_fs ap_fs in
      let ap_fs = Mitl_checking.check debug !Simulating.time_max ap_fs prop in
      match Mitl_checking.eval_at_zero ap_fs with
      | Some res -> fprintf str_formatter "%b\n%!" res
      | None     -> fprintf str_formatter "unknown\n%!";
	  ()
    with
      | _ ->
        fprintf str_formatter "unexpected error\n@."
    end;

	Capd_simulating_stubs.get_dump_data ()


  let hss_process debug auto_length ha =
    let lb = from_string ha in
	hss_process_body debug auto_length lb

  let hss_process_file debug auto_length filename =
    let cin = open_in filename in
    let lb = from_channel cin in
	hss_process_body debug auto_length lb
}}


module Hssweb_app =
  Eliom_registration.App (
    struct
      let application_name = "hssweb"
    end)

let main_service =
  Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

let submission_service = 
  Eliom_service.post_service
	~fallback:main_service
    ~post_params:(string "s1" ** string "s2")
	()

let load_sample_service =
  Eliom_service.service
    ~path:["loadsample"]
    ~get_params:Eliom_parameter.(string "filename")
	()


let get_file =
  Eliom_registration.File.register_service
    ~path:["getfile"]
    ~get_params:unit
    (fun () () -> Lwt.return "../examples/arc.ha")

let sample_link name fn = 
  Html5.D.a load_sample_service [pcdata name] (fn)

let sample_list =
  div ~a:[ a_id "sample_list" ]
    [ div ~a:[ a_id "submenu_header" ] [pcdata "Samples:"];
	  ul ~a:[ a_id "submenu_body" ]
	  [ li [sample_link "Arc" "arc.ha"];
	  	li [sample_link "Disk" "disk.ha"];
	  	li [sample_link "Rotate" "rotate.ha"];
	  	li [sample_link "BB (simple)" "bb-simple.ha"];
	  	li [sample_link "BB (parabola)" "bb-parabola.ha"];
	  ]]

let create_form (v1, v2) =
  fun (n1, n2) ->
    let submenu = div ~a:[ a_id "submenu" ] 
	  [ sample_list ] in

    let left = div ~a:[ a_id "left" ] 
	  [ Html5.D.textarea ~a:[ a_id "spec" ] ~name:n2 ~value:v2 () ] in
    let right = div ~a:[ a_id "right" ]
	  [ div ~a:[ a_id "ctrl" ] [ Html5.D.input ~input_type:`Submit ~value:"Run" () ];
	    Html5.D.textarea ~a:[ a_id "data" ] ~name:n1 ~value:v1 ()
	  ] in
    let input_pane = div ~a:[ a_id "input" ] 
	  [ left; right ] in

    Html5.D.([ submenu; input_pane ])

(*let handler phandler v =
  let p_holder = div ~a:[ a_id "holder" ] [] in
  let f = Html5.D.get_form phandler (create_form p_holder v) in
  Lwt.return
	Html5.F.(html
		(head (title (pcdata "")) 
			[js_script ~uri:(make_uri (Eliom_service.static_dir ())
								["dygraph-combined.js"]) ();
			(*js_script (Dom_html.window##onload <- Dom_html.handler hoge)*)
			])
		(body [ f ]))
*)

(*let holder = div ~a:[ a_id "holder" ] []

{client{
  Eliom_client.onload (fun () -> alert "hoge";
    Js.Unsafe.eval_string "g = new Dygraph(document.getElementById('holder'), [[0,0],[10,10]]);")
}}*)

let gen_frontend phandler (_data, spec) =
  let res = if spec <> "" then hss_process false true spec 
			else "[]" in

  let title_text = "Validated simulator for hybrid automata" in
  (*let p_holder = div ~a:[ a_id "plot" ] [] in*)
  let frm = (Html5.D.post_form phandler (create_form (res, spec)) ()) in

  {unit{
  (*Js.Unsafe.eval_string "g = new Dygraph(document.getElementById('holder'), [[0,0],[10,10]]);"*)
  (*Js.Unsafe.new_obj (Js.Unsafe.variable "Dygraph") [| holder; (Js.Unsafe.eval_string "[[0,0],[10,10]]") |]*)
  (*Js.Unsafe.fun_call (Js.Unsafe.variable "readData") [|Js.Unsafe.inject "holder"; Js.Unsafe.inject "sample"|]*)

  Js.Unsafe.fun_call (Js.Unsafe.variable "plot") [|Js.Unsafe.inject "plot"; Js.Unsafe.eval_string %res|];

  (*let res = %(hss_process_file false true "../examples/arc.ha") in
  Js.Unsafe.fun_call (Js.Unsafe.variable "plot") [|Js.Unsafe.inject "plot"; Js.Unsafe.eval_string res|];*)
  }};
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
				  div ~a:[ a_id "plot" ] []
				];
				frm;
				(*p [pcdata (Format.flush_str_formatter ())];
				a get_file [pcdata "get_file"] ();
				a load_sample_service [pcdata "load_sample"] ("arc.ha"); *)
			  ]]))

let process_service = 
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

let () =
  Hssweb_app.register
    ~service:submission_service
	(fun () (s1,s2) -> gen_frontend submission_service (s1,s2));

  Hssweb_app.register
	~service:load_sample_service

    (fun (fn) () -> 
      let ic = open_in ("../examples/"^fn) in
      try
        let n = in_channel_length ic in
        let s = String.create n in
        really_input ic s 0 n;
        close_in ic;

	    gen_frontend submission_service ("", s);
      with e ->
        close_in_noerr ic;
        raise e);

  Hssweb_app.register
    ~service:main_service
	(fun () () -> gen_frontend submission_service ("", ""))
