let string_avoid_1324 cs =
  let n = String.length cs in

  let is_1324_like a b c d = 
    a < c && c < b && b < d
  in

  let rec check_l i j k =
    if k < n then
      if is_1324_like (int_of_char cs.[i]) (int_of_char cs.[j]) (int_of_char cs.[k]) (int_of_char cs.[k+1])
      then false
      else check_l i j (k + 1)
    else true
  in

  let rec check_k i j =
    if j < n - 2 then
      if not (check_l i j (j + 2))
      then false
      else check_k i (j + 1)
    else true
  in

  let rec check_j i =
    if i < n - 3 then
      if not (check_k i (i + 1))
      then false
      else check_j (i + 1)
    else true
  in

  check_j 0
;;
