x <- readLines("inputs/11.txt") |>
  strsplit(" ", fixed = TRUE) |>
  getElement(1) |>
  as.numeric()

# part one: 186203
update <- function(stones) {
  indiv <- mapply(
    \(n, val) {
      if (val == 0)
        return(rep(1, n))
      len <- nchar(val)
      if (len %% 2 == 0) {
        left <- substr(val, 1, len/2)
        right <- substr(val, len/2 + 1, len)
        res <- as.numeric(c(left, right))
        return(rep(res, n))
      }
      res <- val*2024
      return(rep(res, n))
    },
    stones,
    as.numeric(names(stones))
  )
  table(Reduce(c, indiv, init = numeric()))
}
state <- table(x)
for (n in seq_len(25)) {
  state <- update(state)
}
sum(state)
