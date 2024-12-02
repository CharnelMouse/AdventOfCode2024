readLines("inputs/02.txt") |>
  strsplit(" ", fixed = TRUE) |>
  vapply(
    \(x) {
      x |>
        as.integer() |>
        diff() |>
        (\(y) y*sign(y[[1]]))() |>
        (\(y) all(y >= 1 & y <= 3))()
    },
    logical(1)
  ) |>
  sum()
