x <- readLines("inputs/10.txt") |>
  strsplit("", fixed = TRUE) |>
  lapply(as.integer) |>
  do.call(what = rbind)
starts <- as.data.frame(which(x == 0, arr.ind = TRUE))
trace <- transform(starts, start = seq_len(nrow(starts)), count = 1L)
for (level in seq_len(9)) {
  trace <- rbind(
    transform(trace, row = row - 1L),
    transform(trace, row = row + 1L),
    transform(trace, col = col - 1L),
    transform(trace, col = col + 1L)
  ) |>
    subset(row >= 1 & row <= nrow(x) & col >= 1 & col <= ncol(x)) |>
    transform(height = mapply(\(r, c) x[r, c], row, col)) |>
    subset(height == level) |>
    aggregate(count ~ row + col + start, sum)
}

# part one: 557
nrow(trace)

# part two: 1062
sum(trace$count)
