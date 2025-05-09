type t = Datatype.t * (string -> string list list)

let create dt f = dt, f
let data_type ((dt, _) : t) = dt
let rows_of_input ((_, f) : t) = f
