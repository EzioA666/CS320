let string_init = String.init
let string_length = String.length
let string_get(cs, i0) = String.get cs i0
let stringrev cs =
  let len = string_length cs in
  string_init len (fun i -> string_get(cs, len - 1 - i))