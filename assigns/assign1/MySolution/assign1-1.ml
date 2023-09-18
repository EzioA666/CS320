let intrev10 n =
  let rec helper n acc =
    if n = 0 then acc
    else helper (n / 10) ((acc * 10) + (n mod 10))
  in
  helper n 0