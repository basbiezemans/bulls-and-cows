open Code
open Codemaker

let () = Printf.printf "The secret is: %s\n" (Code.to_string @@ secret ())
