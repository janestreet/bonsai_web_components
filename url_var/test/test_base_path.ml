open! Core
module Base_path = Bonsai_web_url_var.For_testing.Base_path

let with_base_path base_path ~f =
  Base_path.For_testing.set base_path;
  Exn.protect ~f ~finally:Base_path.For_testing.clear
;;

let%expect_test "remove and prepend" =
  let print_path path =
    let removed = Base_path.remove_exn path in
    let prepended = Base_path.prepend path in
    print_s [%message (removed : string) (prepended : string)]
  in
  print_path "/view";
  [%expect {| ((removed /view) (prepended /view)) |}];
  with_base_path "/graphs/" ~f:(fun () ->
    let exact = Base_path.remove_exn "/graphs" in
    let nested = Base_path.remove_exn "/graphs/view" in
    let root = Base_path.prepend "" in
    let nested_url = Base_path.prepend "/view" in
    let outside_raises = Exn.does_raise (fun () -> Base_path.remove_exn "/other/view") in
    print_s
      [%message
        (exact : string)
          (nested : string)
          (root : string)
          (nested_url : string)
          (outside_raises : bool)];
    [%expect
      {|
      ((exact "") (nested /view) (root graphs) (nested_url graphs/view)
       (outside_raises true))
      |}]);
  with_base_path "/" ~f:(fun () ->
    print_path "/view";
    [%expect {| ((removed /view) (prepended /view)) |}])
;;

let%expect_test "remove and prepend preserve suffix slashes" =
  with_base_path "graphs" ~f:(fun () ->
    print_endline (Base_path.prepend (Base_path.remove_exn "/graphs/"));
    [%expect {| graphs/ |}];
    print_endline (Base_path.prepend (Base_path.remove_exn "/graphs//view"));
    [%expect {| graphs//view |}];
    print_endline (Base_path.prepend (Base_path.remove_exn "/graphs"));
    [%expect {| graphs |}])
;;
