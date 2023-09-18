(* ****** ****** *)
(*
Assign1: Onward!

Total: 70 points + 20 bonus points
 
Except for the basic arithmetic functions
(including those on chars), you may only use
the functions in classlib/OCaml/MyOCaml.ml
*)
(* ****** ****** *)

(*
assign1-1: 10 points
Given a natural number n that is not a multiple
of 10, intrev10(n) is the natural number whose
representation in base 10 reverses that of the
number n.

fun intrev10(n: int): int

For instance, if n = 12345, then intrev10(n) is
54321; if n = 10203, then intrev10(n) is 30201.

Please give a TAIL-RECURSIVE implementation of
intrev10.
*)
let intrev10 n =
  let rec helper n acc =
    if n = 0 then acc
    else helper (n / 10) ((acc * 10) + (n mod 10))
  in
  helper n 0
(* ****** ****** *)

(*
assign1-2: 10 points
Given two ordered strings cs1 and cs2, please use
string_make_fwork to implement string_merge(cs1, cs2)
which returns the order string obtained from merging
cs1 and cs2.

For instance, if cs1 = "135" and cs2 = "2468", then
string_merge(cs1)(cs2) equals "1234568"

For instance, if cs1 = "abcde" and cs2 = "1234", then
string_merge(cs1)(cs2) equals "1234abcde"
*)
let string_merge cs1 cs2 =
  let len1 = String.length cs1 and len2 = String.length cs2 in

  let rec merge i j acc =
    if i = len1 then acc ^ (String.sub cs2 j (len2 - j))
    else if j = len2 then acc ^ (String.sub cs1 i (len1 - i))
    else if cs1.[i] <= cs2.[j] then merge (i+1) j (acc ^ (String.make 1 cs1.[i]))
    else merge i (j+1) (acc ^ (String.make 1 cs2.[j]))
  in

  merge 0 0 ""
(* ****** ****** *)

(*
assign1-3: 10 points
A 3-letter sequence abc is 132-like
if a < c < b holds. For instance, 123 is
not 132-like; but 596 is 132-like.

A string is 132-avoid if there is no subsequence
abc in this string that is 132-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 132-avoid;
For instance, 987654321 is 132-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 165 is 132-like.

Please implement a function string_avoid_132 that
checks if a given string is 132-avoid; the function
returns true if and only if the given string is 132-
avoid.

fun string_avoid_132(cs: string): bool
*)
let str_rev s =
  let len = String.length s in
  String.init len (fun i -> s.[len - i - 1])

let intrep_add ds1 ds2 =
  (* Helper function to get a digit from string or 0 if index is out of bounds *)
  let get_digit s i =
      if i < String.length s then
          int_of_char s.[i] - int_of_char '0'
      else
          0
  in
  
  (* Carry out the addition *)
  let rec aux ds1 ds2 i carry acc =
      if i >= String.length ds1 && i >= String.length ds2 && carry = 0 then
          acc
      else
          let sum = get_digit ds1 i + get_digit ds2 i + carry in
          let digit = sum mod 10 in
          let next_carry = sum / 10 in
          aux ds1 ds2 (i+1) next_carry ((string_of_int digit) ^ acc)
  in
  
  (* Remove leading zeros *)
  let rec remove_leading_zeros s =
      if String.length s = 0 then "0"
      else if s.[0] = '0' then remove_leading_zeros (String.sub s 1 ((String.length s) - 1))
      else s
  in
  
  aux (str_rev ds1) (str_rev ds2) 0 0 ""
  |> remove_leading_zeros
;;

(* ****** ****** *)

(*
assign1-5: 20 points

A sequence of chars is ascending if any char in
the sequence is less than or equal to the following
one (when the following one does exist).
Given a string cs, please implement a function
that find the longest ascending subsequence of [cs].
If there are more than one such sequences, the left
most one should be returned.

fun string_longest_ascend(xs: string): string

For instance, given "1324561111", the function
string_longest_ascend returns "13456"

For instance, given "1234561111", the function
string_longest_ascend returns "123456"

For instance, given "1234511111", the function
string_longest_ascend returns "111111".
*)
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
(* ****** ****** *)

(*
assign1-6: 20 bonus points
A 4-letter sequence abcd is 1324-like
if a < c < b < d holds. For instance, 1234 is
not 132-like; but 2547 is 1324-like.

A string is 1324-avoid if there is no subsequence
abc in this string that is 1324-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 1324-avoid;
For instance, 987654321 is 1324-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 1657 is 1324-like.

Please implement a function string_avoid_1324 that
checks if a given string is 1324-avoid; the function
returns true if and only if the given string is 1324-
avoid.

fun string_avoid_1324(cs: string): bool
*)
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


(* ****** ****** *)

(* end of [CS320-2023-Fall-assigns-assign1.ml] *)
