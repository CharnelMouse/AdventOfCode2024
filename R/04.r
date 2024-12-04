x <- readLines("inputs/04.txt")

# part one: 2593
hword <- function(x) {
  sum(vapply(gregexpr("XMAS", x), \(y) sum(y != -1), integer(1))) +
    sum(vapply(gregexpr("SAMX", x), \(y) sum(y != -1), integer(1)))
}
horiz <- hword(x)
grid <- strsplit(x, "", fixed = TRUE) |>
  do.call(what = rbind)
flipped <- grid |>
  t() |>
  apply(1, paste, collapse = "")
vert <- hword(flipped)

nd <- dim(grid)
# diagonals:
# skew up by 3, so (1, -2) to (ncol, ncol-3),
# (1, -1) to (ncol, ncol-2), ...
# (1, nrow-3) to (ncol, nrow + ncol - 4)
start_row <- seq(4L - nd[[2]] + 1L, nd[[1]] - 3L)
skewed <- lapply(
  start_row,
  \(n) {
    res <- character()
    for (col in seq_len(nd[[2]])) {
      row <- n + col - 1L
      if (row %in% seq_len(nd[[1]])) {
        res <- c(res, grid[row, col])
      }
    }
    paste(res, collapse = "")
  }
)
main_diag <- hword(skewed)
rev_start_row <- seq(1L + 3L, nd[[1]] - 3L + nd[[2]] - 1L)
rev_skewed <- lapply(
  rev_start_row,
  \(n) {
    res <- character()
    for (col in seq_len(nd[[2]])) {
      row <- n - col + 1L
      if (row %in% seq_len(nd[[1]])) {
        res <- c(res, grid[row, col])
      }
    }
    paste(res, collapse = "")
  }
)
other_diag <- hword(rev_skewed)
horiz + vert + main_diag + other_diag

# part two: 1950
crosses <- sum(vapply(
  seq_len(nd[[1]] - 2L),
  \(row) {
    sum(vapply(
      seq_len(nd[[2]] - 2L),
      \(col) {
        tri1 <- paste0(
          grid[row, col],
          grid[row + 1, col + 1],
          grid[row + 2, col + 2]
        )
        tri2 <- paste0(
          grid[row + 2, col],
          grid[row + 1, col + 1],
          grid[row, col + 2]
        )
        (tri1 == "MAS" || tri1 == "SAM") && (tri2 == "MAS" || tri2 == "SAM")
      },
      logical(1)
    ))
  },
  integer(1)
))
crosses
