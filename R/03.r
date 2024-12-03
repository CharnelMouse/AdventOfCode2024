x <- readLines("inputs/03.txt")

# part one: 181345830
terms <- regmatches(
  x,
  gregexpr("mul\\(\\d{1,3},\\d{1,3}\\)", x)
) |>
  Reduce(f = c, init = character())
substr(terms, 5, nchar(terms) - 1) |>
  strsplit(",", fixed = TRUE) |>
  vapply(\(pair) prod(as.integer(pair)), numeric(1)) |>
  sum()

# part two: 98729041
allx <- paste(x, collapse = "")
allterms <- regmatches(
  allx,
  gregexpr("do\\(\\)|don't\\(\\)|mul\\(\\d{1,3},\\d{1,3}\\)", allx)
)[[1]]
to_enabled <- function(y) {
  used <- integer()
  current <- 0L
  starts <- which(y == "do()")
  ends <- c(which(y == "don't()"), length(y) + 1L)
  while (length(ends) > 0) {
    used <- c(used, current:(ends[[1]] - 1L))
    starts <- starts[starts > ends[[1]]]
    if (length(starts) == 0)
      break
    ends <- ends[ends > starts[[1]]]
    current <- starts[[1]]
  }
  y[used][startsWith(y[used], "mul")]
}
tst <- function(allterms) {
  enabled <- to_enabled(allterms) |>
    Reduce(f = c, init = character())
  substr(enabled, 5, nchar(enabled) - 1) |>
    strsplit(",", fixed = TRUE) |>
    vapply(\(pair) prod(as.integer(pair)), numeric(1)) |>
    sum()
}
tst(allterms)
