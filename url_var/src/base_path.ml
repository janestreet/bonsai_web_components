open! Core

let normalize content =
  let content = String.strip content in
  let content = String.chop_prefix_if_exists content ~prefix:"/" in
  let content = String.chop_suffix_if_exists content ~suffix:"/" in
  Option.some_if (not (String.is_empty content)) content
;;

let read_from_dom () =
  Option.try_with_join
  @@ fun () ->
  if not Am_running_how_js.(am_in_browser_like_api am_running_how)
  then None
  else
    let open Js_of_ocaml in
    let window = Browser_expert.Global.window_exn () in
    let document = window##.document in
    match
      document##querySelector ~selectors:(Js.string "base[href]") |> Js.Opt.to_option
    with
    | None -> None
    | Some _ ->
      (* Without an explicit [base], [baseURI] is the current page, not a mount point. *)
      let base_uri = document##.baseURI |> Js.to_string |> Uri.of_string in
      let page_uri = window##.location##.href |> Js.to_string |> Uri.of_string in
      let origin uri = Uri.scheme uri, Uri.host uri, Uri.port uri in
      let same_origin =
        [%equal: string option * string option * int option]
          (origin base_uri)
          (origin page_uri)
      in
      let path = Uri.path base_uri in
      if same_origin && String.is_suffix path ~suffix:"/" then normalize path else None
;;

let override_for_testing = ref None

let get () =
  match !override_for_testing with
  | Some override -> override
  | None -> read_from_dom ()
;;

let remove_exn path =
  match get () with
  | None -> path
  | Some base_path ->
    let path = String.chop_prefix_if_exists path ~prefix:"/" in
    if String.equal path base_path
    then ""
    else (
      match String.chop_prefix path ~prefix:(base_path ^ "/") with
      | Some path -> "/" ^ path
      | None ->
        raise_s
          [%message
            "URL path does not start with the configured base path"
              (base_path : string)
              (path : string)])
;;

let prepend path =
  match get () with
  | None -> path
  | Some base_path ->
    if String.is_empty path || String.is_prefix path ~prefix:"/"
    then base_path ^ path
    else [%string "%{base_path}/%{path}"]
;;

module For_testing = struct
  let set base_path = override_for_testing := Some (normalize base_path)
  let clear () = override_for_testing := None
end
