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

let is_valid code =
  let is_valid digit = in_range digit (1, 6) in
  List.length code == 4 && List.for_all is_valid code