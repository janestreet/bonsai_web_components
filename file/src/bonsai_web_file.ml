open Core
open Bonsai_web
open Bonsai.Let_syntax

module Progress = struct
  type t =
    { loaded : int
    ; total : int
    }
  [@@deriving compare, equal, sexp]

  let to_percentage { loaded; total } = Percent.of_mult (float loaded /. float total)
end

module _ = struct
  type t =
    | Contents of Bigstring.t
    | Loading of Progress.t option
    | Error of Error.t
  [@@deriving compare, equal, sexp]
end

module Read_error = struct
  type t =
    | Aborted
    | Error of Error.t
  [@@deriving compare, equal, sexp]
end

module File_read = struct
  type t =
    { result : (Bigstring.t, Read_error.t) Result.t Ui_effect.t
    ; abort : unit Ui_effect.t
    }
  [@@deriving fields ~getters]
end

type t =
  { read : (Progress.t -> unit Ui_effect.t) -> File_read.t Ui_effect.t
  ; filename : string
  }
[@@deriving fields ~getters ~iterators:create]

let sexp_of_t t = Sexp.Atom [%string "<file %{filename t#String}>"]
let read ?(on_progress = fun _progress -> Ui_effect.Ignore) t = t.read on_progress

let contents t =
  let open Ui_effect.Let_syntax in
  match%map read t >>= File_read.result with
  | Ok contents -> Ok contents
  | Error Aborted -> assert false
  | Error (Error e) -> Error e
;;

module Expert = struct
  type file_read = File_read.t =
    { result : (Bigstring.t, Read_error.t) Result.t Ui_effect.t
    ; abort : unit Ui_effect.t
    }

  let create = Fields.create
end

module For_testing = struct
  module Test_data = struct
    type data =
      | Closed of Bigstring.t Or_error.t
      | Open of
          { chunks : string Queue.t
          ; total_bytes : int
          }

    type read_callbacks =
      { on_progress : Progress.t -> unit
      ; on_finished : Bigstring.t Or_error.t -> unit
      }

    module Read_state = struct
      type t =
        | Not_reading
        | Aborted
        | Reading of read_callbacks

      let iter t ~f =
        match t with
        | Not_reading | Aborted -> ()
        | Reading callbacks -> f callbacks
      ;;
    end

    type t =
      { filename : string
      ; mutable data : data
      ; mutable read_state : Read_state.t
      }

    let create_stream ~filename ~total_bytes =
      { filename
      ; data = Open { chunks = Queue.create (); total_bytes }
      ; read_state = Not_reading
      }
    ;;

    let create_static ~filename ~contents =
      { filename
      ; data = Closed (Ok (Bigstring.of_string contents))
      ; read_state = Not_reading
      }
    ;;

    let read_status t =
      match t.read_state with
      | Not_reading -> `Not_reading
      | Aborted -> `Aborted
      | Reading _ -> `Reading
    ;;

    let read t read =
      (match t.data with
       | Open { chunks; total_bytes } ->
         read.on_progress
           { Progress.loaded = Queue.sum (module Int) chunks ~f:String.length
           ; total = total_bytes
           }
       | Closed result -> read.on_finished result);
      t.read_state <- Reading read
    ;;

    let abort_read t = t.read_state <- Aborted

    let feed_exn t chunk =
      match t.data with
      | Open { chunks; total_bytes } ->
        Queue.enqueue chunks chunk;
        let progress =
          { Progress.loaded = Queue.sum (module Int) chunks ~f:String.length
          ; total = total_bytes
          }
        in
        Read_state.iter t.read_state ~f:(fun read -> read.on_progress progress)
      | Closed _ -> raise_s [%message "Bonsai_web_file.Test_data.feed: already closed"]
    ;;

    let close t =
      match t.data with
      | Closed _ -> ()
      | Open { chunks; total_bytes = _ } ->
        let result = Bigstring.create (Queue.sum (module Int) chunks ~f:String.length) in
        let len =
          Queue.fold ~init:0 chunks ~f:(fun dst_pos src ->
            Bigstring.From_string.blit
              ~src
              ~src_pos:0
              ~dst:result
              ~dst_pos
              ~len:(String.length src);
            dst_pos + String.length src)
        in
        assert (Bigstring.length result = len);
        let result = Ok result in
        t.data <- Closed result;
        Read_state.iter t.read_state ~f:(fun read -> read.on_finished result)
    ;;

    let close_error t error =
      match t.data with
      | Closed _ -> ()
      | Open _ ->
        let result = Error error in
        t.data <- Closed result;
        Read_state.iter t.read_state ~f:(fun read -> read.on_finished result)
    ;;
  end

  let create test_data =
    let module Svar = Ui_effect.For_testing.Svar in
    let read on_progress =
      let (result_var : (Bigstring.t, Read_error.t) Result.t Svar.t) = Svar.create () in
      let result =
        Ui_effect.For_testing.of_svar_fun
          (fun () ->
            Test_data.read
              test_data
              { on_progress =
                  (fun progress ->
                    on_progress progress
                    |> Ui_effect.Expert.handle ~on_exn:(fun exn ->
                      Exn.reraise exn "Unhandled exception raised in effect"))
              ; on_finished =
                  (fun result ->
                    Svar.fill_if_empty
                      result_var
                      (Result.map_error result ~f:(fun e -> Read_error.Error e)))
              };
            result_var)
          ()
      in
      let abort =
        Ui_effect.of_sync_fun
          (fun () ->
            Test_data.abort_read test_data;
            Svar.fill_if_empty result_var (Error Aborted))
          ()
      in
      { File_read.result; abort }
    in
    { read = Ui_effect.of_sync_fun read; filename = test_data.filename }
  ;;
end

module Read_on_change = struct
  module File = struct
    type nonrec t = (t[@sexp.opaque]) [@@deriving sexp]

    let equal = phys_equal
  end

  module File_read' = struct
    type t = (File_read.t[@sexp.opaque]) [@@deriving sexp]

    let equal = phys_equal
  end

  module Status = struct
    type t =
      | Starting
      | In_progress of Progress.t
      | Complete of Bigstring.t Or_error.t
    [@@deriving compare, equal, sexp]
  end

  module File_state = struct
    type t =
      | Before_first_read
      | Reading of
          { filename : string
          ; file_read : File_read'.t
          ; status : Status.t
          }
    [@@deriving equal, sexp]

    module Action = struct
      type t =
        | Reset
        | Start_read of
            { filename : string
            ; file_read : File_read'.t
            }
        | Set_status of Status.t
      [@@deriving equal, sexp]
    end

    let apply_action context t (action : Action.t) =
      let abort_previous () =
        match t with
        | Before_first_read -> ()
        | Reading { filename = _old_filename; file_read = old_file_read; status = _ } ->
          Bonsai.Apply_action_context.schedule_event
            context
            (File_read.abort old_file_read)
      in
      match action with
      | Reset ->
        abort_previous ();
        Before_first_read
      | Start_read { filename; file_read } ->
        abort_previous ();
        Reading { filename; file_read; status = Starting }
      | Set_status status ->
        (match t with
         | Before_first_read -> t
         | Reading { filename; file_read; status = _ } ->
           Reading { filename; file_read; status })
    ;;

    let abort_read_if_applicable t (local_ _graph) =
      match%sub t with
      | Before_first_read -> return Ui_effect.Ignore
      | Reading { filename = _; file_read; status = _ } -> file_read >>| File_read.abort
    ;;
  end

  let create_single_opt file (local_ graph) =
    let state, inject =
      Bonsai.state_machine
        ~sexp_of_model:[%sexp_of: File_state.t]
        ~equal:[%equal: File_state.t]
        ~sexp_of_action:[%sexp_of: File_state.Action.t]
        ~default_model:Before_first_read
        ~apply_action:File_state.apply_action
        graph
    in
    let () =
      let abort = File_state.abort_read_if_applicable state graph in
      Bonsai.Edge.lifecycle ~on_deactivate:abort graph
    in
    let () =
      Bonsai.Edge.on_change
        ~trigger:`After_display
        ~sexp_of_model:[%sexp_of: File.t option]
        ~equal:[%equal: File.t option]
        file
        ~callback:
          (let%map inject in
           function
           | None -> inject Reset
           | Some file ->
             let open Ui_effect.Let_syntax in
             let%bind file_read =
               read file ~on_progress:(fun progress ->
                 inject (Set_status (In_progress progress)))
             in
             let%bind () =
               let filename = file.filename in
               inject (Start_read { filename; file_read })
             in
             (match%bind File_read.result file_read with
              | Error Aborted ->
                (* Let the next read take over *)
                return ()
              | Error (Error e) -> inject (Set_status (Complete (Error e)))
              | Ok contents -> inject (Set_status (Complete (Ok contents)))))
        graph
    in
    let%arr state and file in
    let%map.Option file in
    match state with
    | Before_first_read ->
      (* We should try to minimize using [file.filename], since the filename can update
         before the [Bonsai.Edge.on_change] fires above. This can trigger the filename to
         change before the content is refreshed when a new file is uploaded. *)
      file.filename, Status.Starting
    | Reading { filename; status; _ } -> filename, status
  ;;

  let create_single file (local_ graph) =
    let%arr file
    and filename_and_status = create_single_opt (file >>| Option.some) graph in
    Option.value filename_and_status ~default:(file.filename, Starting)
  ;;

  let create_multiple files (local_ graph) =
    (* In reality, I suspect that whenever the user changes their selection in a file
       picker widget, the browser generates an entirely new set of File objects for us. So
       I suspect it's not possible for [files] to change in such a way that some, but not
       all, of the keys change. However, it's easy enough to support that, so we do.

       The one thing we don't support is if a file disappears from the map and then comes
       back. In that case, we've already told the file reader to abort the read when it
       disappeared, so there is no way for us to recover. *)
    Bonsai.assoc
      (module Filename)
      files
      ~f:(fun _filename file (local_ graph) ->
        let%arr _filename, status = create_single file graph in
        status)
      graph
  ;;
end
