type feedback = {
  bulls : int;
  cows : int;
}

val feedback : Code.code -> Code.code -> feedback

val show : feedback -> string
