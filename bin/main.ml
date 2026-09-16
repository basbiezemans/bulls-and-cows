open Printf
open Codemaker

type game_result =
  | ExitGame
  | GameOver
  | GameWon of int

let read_with_prompt prompt =
  print_string prompt;
  flush stdout;
  read_line ()
  |> String.trim
  |> String.lowercase_ascii

let rec play_again prompt =
  let answer = read_with_prompt prompt in
  match answer with
  | "y" | "yes" -> true
  | "n" | "no"  -> false
  | _otherwise  -> play_again "Please enter 'y' or 'n': "

let pluralize singular plural count =
  match count with
  | 1 -> "1 " ^ singular
  | _ -> sprintf "%d %s" count plural

let rec play_game limit secret turns_left =
  let turns = pluralize "turn" "turns" turns_left in
  printf "\nYou have %s left.\n" turns;
  let input = read_with_prompt "Guess: " in
  if input = "quit" || input = "exit" then
    ExitGame
  else
    match Code.of_string input with
    | Error msg ->
      printf "Error: %s\n" msg;
      play_game limit secret turns_left
    | Ok guess ->
      if guess = secret then
        GameWon (limit - turns_left + 1)
      else if turns_left = 1 then
        GameOver
      else begin
        printf "Hint: %s\n" (show (feedback secret guess));
        play_game limit secret (turns_left - 1)
      end

let rec main () =
  Sys.command "clear" |> ignore; (* screen clearing is best-effort *)
  print_endline "";
  print_endline "┌──────────────────────────────────────┐";
  print_endline "│ Bulls & Cows, the code-breaking game │";
  print_endline "├──────────────────────────────────────┤";
  print_endline "│ >> Type 'quit' or 'exit' to stop     │";
  print_endline "└──────────────────────────────────────┘";
  let limit = 10
  and secret = Code.secret () in
  printf "You have %d turns to break the code. Good luck!\n" limit;
  match play_game limit secret limit with
  | ExitGame ->
    printf "\nThanks for playing! The secret was %s\n" (Code.to_string secret)
  | GameOver ->
    printf "No such luck. The secret was %s\n" (Code.to_string secret)
  | GameWon num_turns ->
    printf "\nYou won in %s!\n" (pluralize "guess" "guesses" num_turns);
    if play_again "Do you want to play again? (y/n): " then main () else ()

let () =
  Random.self_init (); (* ensure the RNG is not deterministic across runs *)
  try
    main ()
  with
  | End_of_file ->
    print_endline "\nInput closed. Thanks for playing!"