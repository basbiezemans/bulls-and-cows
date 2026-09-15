open Batteries

(**
 * [random_code n range lst] generates a list of [n] random integers in the
 * [range] (min, max).
 * @param n number of random integers
 * @param range the min..max range (inclusive)
 * @param lst an initial list
 * @return a list of random integers
 *)
let rec random_code n range lst =
  if List.length lst >= n then
    lst
  else
    let (min, max) = range in
    random_code n range (Random.int_in_range ~min:min ~max:max :: lst)

let code_to_string = Fun.compose (String.concat "") (List.map Int.to_string)

let secret = random_code 4 (1, 6) []

let in_range n range =
  let (min, max) = range in min <= n && n <= max

let int_of_digit c =
  if in_range c ('0', '9') then
    Some (Char.code c - Char.code '0')
  else
    None

let is_valid code =
  let in_range = Fun.flip in_range in
  List.length code = 4 && List.for_all (in_range (1, 6)) code

let string_to_code s =
  if String.length s < 4 then
    Error "please enter 4 digits"
  else
    let code = String.sub s 0 4
      |> String.to_list
      |> List.map int_of_digit
      |> List.map (Option.default 0) in
    if is_valid code then
      Ok code
    else
      Error "each digit should be between 1 and 6, e.g. 1236"

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

type feedback = {
  bulls : int;
  cows  : int
}

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