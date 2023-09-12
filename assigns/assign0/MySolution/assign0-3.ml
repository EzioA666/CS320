let chr = Char.chr
let ord = Char.code
let str(c0) = String.make 1 c0

let int2str i0 =
  let rec loop i acc = 
    if i = 0 && acc = "" then "0" (* Special case when i0 is 0 *)
    else if i = 0 then acc
    else 
      let digit = i mod 10 in
      let next_char = chr ((ord '0') + digit) in
      loop (i / 10) (str(next_char) ^ acc)
  in
  if i0 < 0 then "-" ^ loop (-i0) "" else loop i0 ""