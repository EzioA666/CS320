let string_init = String.init
let string_length = String.length
let string_get(cs, i0) = String.get cs i0
let stringrev cs = 
  string_init (string_length cs) (fun i -> string_get(cs, (string_length cs) - 1 - i))(* Initialize reversed string *)