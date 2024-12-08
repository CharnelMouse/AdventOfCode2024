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
antinodes <- lapply(
  antennae,
  \(x) {
    lapply(
      seq_len(nrow(x) - 1),
      \(n) {
        lapply(
          setdiff(seq_len(nrow(x)), seq_len(n)),
          \(m) {
            p1 <- x[n, ]
            p2 <- x[m, ]
            matrix(
              c(2L*p1 - p2, 2L*p2 - p1),
              ncol = 2,
              byrow = TRUE
            )
          }
        ) |>
          do.call(what = rbind)
      }
    ) |>
      do.call(what = rbind) |>
      unique()
  }
)
uniq <- unique(do.call(rbind, antinodes))
nrow(uniq[apply(uniq, 1, \(pos) all(pos >= 1 & pos <= dim(x))), ]) # part one: 348
