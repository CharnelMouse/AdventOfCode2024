x <- readLines("inputs/12.txt") |>
  strsplit("", fixed = TRUE) |>
  do.call(what = rbind)

# part one: 1477762
plants <- sort(unique(as.vector(x)))
plant_cells <- lapply(
  setNames(nm = plants),
  \(p) which(x == p, arr.ind = TRUE) |>
    as.data.frame() |>
    sort_by(~ row + col)
)
block <- function(cells) {
  cells <- cbind(cells, block = NA_integer_, neighbours = NA_integer_)
  cells <- sort_by(cells, ~ row + col)
  for (n in seq_len(nrow(cells))) {
    cell <- cells[n, c("row", "col")]
    neighbours <- rbind(
      cell,
      cell + c(-1L, 0L),
      cell + c(1L, 0L),
      cell + c(0L, -1L),
      cell + c(0L, 1L)
    )
    matched_neighbours <- merge(neighbours, cells[c("row", "col", "block")])
    n_neighbours <- nrow(matched_neighbours) - 1L
    if (all(is.na(matched_neighbours$block))) {
      matched_neighbours$new_block <- max(c(0L, cells$block), na.rm = TRUE) + 1L
    }else{
      matched_neighbours <- unique(rbind(
        matched_neighbours,
        subset(cells, block %in% na.omit(matched_neighbours$block), c(row, col, block))
      ))
      matched_neighbours$new_block <- min(matched_neighbours$block, na.rm = TRUE)
    }
    cells <- merge(cells, matched_neighbours, all.x = TRUE, sort = FALSE) |>
      sort_by(~ row + col)
    cells$block <- ifelse(is.na(cells$new_block), cells$block, cells$new_block)
    cells$new_block <- NULL
    cells[[n, "neighbours"]] <- n_neighbours
  }
  cells
}
plant_blocks <- lapply(plant_cells, block)
price <- function(cells) {
  cells |>
    transform(
      perimeter = 4L - neighbours,
      area = 1L
    ) |>
    aggregate(cbind(perimeter, area) ~ block, sum) |>
    with(sum(perimeter * area))
}
sum(vapply(plant_blocks, price, integer(1)))
