    let isPrime n0 =
      let rec check_divisor x d =
          if d * d > x then true  (* If d exceeds sqrt(x), x is prime *)
          else if x mod d = 0 then false  (* If x is divisible by d, it's not prime *)
          else check_divisor x (d+1)  
      in
      n0 > 1 && check_divisor n0 2