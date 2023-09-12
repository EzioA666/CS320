let myans =
  let rec fact_accum x acc = 
    if acc = 0 then x - 1 (* Return previous value of x when acc overflows to 0 *)
    else if x > 0 then fact_accum (x + 1) (x * acc)
    else 0
  in
  fact_accum 1 1