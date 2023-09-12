let chr = Char.chr
let ord = Char.code
let str(c0) = String.make 1 c0
;;
(* ****** ****** *)

let string_init = String.init
let string_length = String.length
let string_get(cs, i0) = String.get cs i0
;;

let int2str i0 =
  if i0 = 0 then "0"
  else
      let rec loop n acc =
          if n = 0 then acc
          else loop (n / 10) ((chr (48 + (n mod 10))) :: acc)
      in
      let chars = loop i0 [] in
      string_init (List.length chars) (fun idx -> List.nth chars idx)
;;
