let string_merge cs1 cs2 =
  let len1 = String.length cs1 and len2 = String.length cs2 in

  let rec merge i j acc =
    if i = len1 then acc ^ (String.sub cs2 j (len2 - j))
    else if j = len2 then acc ^ (String.sub cs1 i (len1 - i))
    else if cs1.[i] <= cs2.[j] then merge (i+1) j (acc ^ (String.make 1 cs1.[i]))
    else merge i (j+1) (acc ^ (String.make 1 cs2.[j]))
  in

  merge 0 0 ""