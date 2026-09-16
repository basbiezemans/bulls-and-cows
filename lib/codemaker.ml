open Batteries

type feedback = {
  bulls : int;
  cows  : int
}

let equal pairs = List.filter (fun p -> fst p = snd p) pairs

let unequal pairs = List.filter (fun p -> fst p != snd p) pairs

let num_bulls pairs = List.length (equal pairs)

let count_cows (n, digits) x =
  if List.mem x digits then
    (n + 1, List.remove digits x)
  else
    (n, digits)

let num_cows pairs =
  let (code1, code2) = List.split (unequal pairs) in
  List.fold_left count_cows (0, code2) code1 |> fst

(**
 * [feedback code1 code2] returns the number of matching digits that are in
 * the right positions (bulls), and in different positions (cows).
 * @param code1 a list of 4 digits
 * @param code2 a list of 4 digits
 * @return a record with the number of bulls and cows
 *)
let feedback code1 code2 =
  let pairs = List.combine code1 code2 in
  { bulls = num_bulls pairs; cows = num_cows pairs }

let show { bulls; cows } =
  (String.repeat "●" bulls) ^ (String.repeat "○" cows)