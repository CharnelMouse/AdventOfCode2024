x <- readLines("inputs/10.txt") |>
  strsplit("", fixed = TRUE) |>
  lapply(as.integer) |>
  do.call(what = rbind)

# part one: 557
ends <- as.data.frame(which(x == 9, arr.ind = TRUE))
trace <- transform(
  ends,
  ends = seq_len(nrow(ends))
)
for (level in 8:0) {
  candidates <- rbind(
    transform(trace, row = row - 1L),
    transform(trace, row = row + 1L),
    transform(trace, col = col - 1L),
    transform(trace, col = col + 1L)
  ) |>
    subset(row >= 1 & row <= nrow(x) & col >= 1 & col <= ncol(x))
  trace <- candidates |>
    transform(height = mapply(\(r, c) x[r, c], row, col)) |>
    subset(height == level) |>
    transform(ends = vapply(ends, toString, character(1))) |>
    aggregate(ends ~ row + col, \(x) unique(unlist(strsplit(x, ", ", fixed = TRUE))))
}
trace <- trace |>
  transform(count = lengths(ends))
sum(trace$count)

# part two: 1062
starts <- as.data.frame(which(x == 0, arr.ind = TRUE))
trace2 <- transform(
  starts,
  count = 1L
)
for (level in 1:9) {
  candidates <- rbind(
    transform(trace2, row = row - 1L),
    transform(trace2, row = row + 1L),
    transform(trace2, col = col - 1L),
    transform(trace2, col = col + 1L)
  ) |>
    subset(row >= 1 & row <= nrow(x) & col >= 1 & col <= ncol(x))
  trace2 <- candidates |>
    transform(height = mapply(\(r, c) x[r, c], row, col)) |>
    subset(height == level) |>
    aggregate(count ~ row + col, sum)
}
sum(trace2$count)
