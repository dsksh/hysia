{shared{
  open Eliom_lib
  open Eliom_content
  open Eliom_content.Html5.D.Raw
  open Eliom_parameter
  open Printf
}}

module Hssweb_app =
  Eliom_registration.App (
    struct
      let application_name = "hssweb"
    end)

let main_service =
  Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

let create_form (v1, v2) =
  fun (n1, n2) ->
    Html5.D.([ textarea ~a:[ a_id "data_holder" ] ~name:n1 ~value:v1 ();
			   textarea ~a:[ a_id "spec_holder" ] ~name:n2 ~value:v2 ();
			   input ~input_type:`Submit ~value:"Submit" () ])

let handler phandler v =
  let f = Html5.D.get_form phandler (create_form v) in
  Lwt.return
	Html5.F.(html
		(head (title (pcdata "")) [])
		(body [ div ~a:[ a_id "holder" ] [];
				f ]))

let post_handler phandler v =
  let f = (Html5.D.post_form phandler (create_form v) ()) in
  Hss.Util.report Format.str_formatter Hss.Util.SyntaxError;
  Lwt.return
	Html5.F.(html
		(head (title (pcdata "")) [])
		(body [ div ~a:[ a_id "holder" ] [];
				p [pcdata (Format.flush_str_formatter ())];
				f ]))

let submission_service = 
  Eliom_service.post_service
	~fallback:main_service
    ~post_params:(string "s1" ** string "s2")
	()

let () =
  Hssweb_app.register
    ~service:submission_service
	(fun () (s1,s2) -> post_handler submission_service (s2,s1));
  Hssweb_app.register
    ~service:main_service
	(fun () () -> post_handler submission_service ("", "hoge"))
