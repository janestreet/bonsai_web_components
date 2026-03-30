type t = Datatype.t * (?headers:string list -> string list list -> string)

let create dt f = dt, f
let data_type ((dt, _) : t) = dt
let rows_to_output ((_, f) : t) = f
