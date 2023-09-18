let string_longest_ascend xs =
  let n = String.length xs in
  if n = 0 then ""
  else
      (* Initialize with first char *)
      let current_seq_start = ref 0 in
      let current_seq_length = ref 1 in
      let longest_seq_start = ref 0 in
      let longest_seq_length = ref 1 in
      
      for i = 1 to n - 1 do
          (* If the current character is greater than or equal to the last character of current sequence *)
          if xs.[i] >= xs.[i - 1] then
              incr current_seq_length
          else
              (* Check if the current sequence is longer than the longest sequence *)
              if !current_seq_length > !longest_seq_length then begin
                  longest_seq_length := !current_seq_length;
                  longest_seq_start := !current_seq_start;
              end;
              current_seq_length := 1;
              current_seq_start := i;
      done;

      (* Check once more after the loop in case the longest sequence is at the end *)
      if !current_seq_length > !longest_seq_length then begin
          longest_seq_length := !current_seq_length;
          longest_seq_start := !current_seq_start;
      end;

      String.sub xs !longest_seq_start !longest_seq_length