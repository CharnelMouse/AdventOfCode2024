sorted <- scan("inputs/01.txt", list("", "")) |>
  lapply(as.integer) |>
  lapply(sort)
sum(mapply(\(x, y) abs(x - y), sorted[[1]], sorted[[2]])) # part one: 2742123
matches <- outer(sorted[[1]], sorted[[2]], `==`)
sum(matches*sorted[[1]]) # part two: 21328497