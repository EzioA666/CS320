let string_avoid_132 cs =
  let stack = Stack.create () in
  let a = ref (int_of_char '0') in  (* initialize with a value outside normal ASCII range *)
  
  for i = 0 to (String.length cs) - 1 do
    let current = int_of_char cs.[i] in
    while not (Stack.is_empty stack) && (Stack.top stack) > current do
      a := Stack.pop stack;
    done;

    if not (Stack.is_empty stack) && current < (Stack.top stack) && current > !a then
      (* We found a 132-like sequence *)
      exit 0 (* Exiting the loop early as we found the sequence *)
  else begin
      Stack.push current stack;
  end;
done