type digit = D1 | D2 | D3 | D4 | D5 | D6

type code = digit * digit * digit * digit

val to_list : code -> digit list

val of_string : string -> (code, string) result

val to_string : code -> string

val secret : unit -> code
