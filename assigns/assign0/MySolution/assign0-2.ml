let isPrime n0 =
  if n0 <= 1 then false
  else if n0 <= 3 then true
  else if n0 mod 2 = 0 || n0 mod 3 = 0 then false
  else
    let rec test_divisor d =
      if d * d > n0 then true
      else if n0 mod d = 0 || n0 mod (d + 2) = 0 then false
      else test_divisor (d + 6) (* 6k +/- 1 pattern *)
    in
    test_divisor 5 (* Starts testing from 5 *)