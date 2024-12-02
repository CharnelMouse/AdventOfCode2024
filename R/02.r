x <- readLines("inputs/02.txt") |>
  strsplit(" ", fixed = TRUE) |>
  lapply(as.integer)
safe <- function(x) {
  x <- diff(x)
  y <- x*sign(x[[1]])
  all(y >= 1 & y <= 3)
}
sum(vapply(x, safe, logical(1))) # part one: 407
damp_safe <- function(x) {
  any(vapply(seq_along(x), \(n) safe(x[-n]), logical(1)))
}
sum(vapply(x, damp_safe, logical(1)))
