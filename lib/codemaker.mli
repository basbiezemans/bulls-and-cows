type feedback = {
  bulls : int;
  cows : int;
}

val feedback : int list -> int list -> feedback

val show : feedback -> string
