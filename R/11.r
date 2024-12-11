x <- readLines("inputs/11.txt") |>
  strsplit(" ", fixed = TRUE) |>
  getElement(1) |>
  as.numeric()

# part one: 186203
update <- function(stones) {
  indiv <- mapply(
    \(n, val) {
      if (val == 0)
        return(setNames(n, 1))
      len <- nchar(val)
      if (len %% 2 == 0) {
        left <- substr(val, 1, len/2)
        right <- substr(val, len/2 + 1, len)
        res <- as.numeric(c(left, right))
        return(setNames(c(n, n), res))
      }
      res <- val*2024
      return(setNames(n, res))
    },
    stones,
    as.numeric(names(stones))
  )
  freqs <- Reduce(c, indiv, init = integer())
  tapply(freqs, names(freqs), sum)
}
state <- table(x)
for (n in seq_len(25)) {
  state <- update(state)
}
sum(state)

# part two: 221291560078593
for (n in seq_len(50)) {
  state <- update(state)
}
sum(state)
