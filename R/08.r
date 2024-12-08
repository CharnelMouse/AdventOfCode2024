x <- readLines("inputs/08.txt") |>
  strsplit("", fixed = TRUE) |>
  do.call(what = rbind)
freqs <- unique(x[x != "."])
antennae <- lapply(
  setNames(nm = freqs),
  \(freq) {
    which(x == freq, arr.ind = TRUE)
  }
)
calculate_antinodes <- function(map, antennae, pairwise_antinodes) {
  lapply(
    antennae,
    \(x) {
      lapply(
        seq_len(nrow(x) - 1),
        \(n) {
          lapply(
            setdiff(seq_len(nrow(x)), seq_len(n)),
            \(m) {
              pairwise_antinodes(map, x[n, ], x[m, ])
            }
          ) |>
            do.call(what = rbind)
        }
      ) |>
        do.call(what = rbind) |>
        unique()
    }
  ) |>
    do.call(what = rbind) |>
    unique()
}

# part one: 348
pairwise_simple_antinodes <- function(x, p1, p2) {
  res <- matrix(
    c(2L*p1 - p2, 2L*p2 - p1),
    ncol = 2,
    byrow = TRUE
  )
  res[apply(res, 1, \(y) all(y >= 1 & y <= dim(x))), , drop = FALSE]
}
simple_antinodes <- calculate_antinodes(x, antennae, pairwise_simple_antinodes)
nrow(simple_antinodes)

# part two:
pairwise_all_antinodes <- function(x, p1, p2) {
  diff <- p2 - p1
  res <- matrix(integer(), ncol = 2)
  # forward
  pos <- p2
  while (all(pos >= 1 & pos <= dim(x))) {
    res <- rbind(res, pos)
    pos <- pos + diff
  }
  # backward
  pos <- p1
  while (all(pos >= 1 & pos <= dim(x))) {
    res <- rbind(res, pos)
    pos <- pos - diff
  }
  res
}
all_antinodes <- calculate_antinodes(x, antennae, pairwise_all_antinodes)
nrow(all_antinodes)
