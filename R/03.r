x <- readLines("inputs/03.txt")
terms <- regmatches(
  x,
  gregexpr("mul\\(\\d{1,3},\\d{1,3}\\)", x)
) |>
  Reduce(f = c, init = character())
# part one: 181345830
substr(terms, 5, nchar(terms) - 1) |>
  strsplit(",", fixed = TRUE) |>
  vapply(\(pair) prod(as.integer(pair)), numeric(1)) |>
  sum()
