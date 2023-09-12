
let chr = Char.chr
let ord = Char.code
let str(c0) = String.make 1 c0
let string_init = String.init
let string_length = String.length
let string_get(cs, i0) = String.get cs i0

let str2int cs =
  let len = string_length cs in
  let rec loop i acc = 
    if i = len then acc
    else
      let char_val = ord (string_get(cs, i)) in
      let int_val = char_val - (ord '0') in
      loop (i + 1) (acc * 10 + int_val)
  in
  if len = 0 then 0 (* handle empty string *)
  else if string_get(cs, 0) = '-' then - (loop 1 0)
  else loop 0 0