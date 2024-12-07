x <- readLines("inputs/07.txt") |>
  strsplit(":? ") |>
  lapply(as.numeric)
tests <- vapply(x, `[[`, numeric(1), 1)
inputs <- lapply(x, `[`, -1)
len <- length(tests)

# part one: 663613490587
poss <- function(ins, test, ops = list(`+`, `*`)) {
  if (length(ins) == 0)
    return(0)
  res <- ins[[1]]
  rem <- ins[-1]
  res <- Reduce(
    \(state, nxt) {
      x <- Reduce(c, lapply(ops, \(op) op(state, nxt)), init = numeric())
      x[x <= test]
    },
    rem,
    init = res
  )
  any(res == test)
}
sum(tests[mapply(poss, inputs, tests)])

# part two: 110365987435001
concat <- function(x, y) as.numeric(paste0(as.character(x), as.character(y)))
sum(tests[mapply(poss, inputs, tests, MoreArgs = list(ops = list(`+`, `*`, concat)))])
