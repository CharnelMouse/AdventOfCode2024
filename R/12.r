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
  cells <- cbind(
    cells,
    block = rep(NA_integer_, nrow(cells)),
    neighbours = rep(NA_integer_, nrow(cells))
  )
  cells$neighbour_pos <- rep(list(NULL), nrow(cells))
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
    neighbour_pos <- subset(
      matched_neighbours,
      row != cell[[1]] | col != cell[[2]],
      c(row, col)
    )
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

# part two: 923480
blocks <- Map(
  \(bl, nm) cbind(data.frame(plant = nm), bl),
  plant_blocks,
  names(plant_blocks)
) |>
  do.call(what = rbind)
area <- tapply(blocks, blocks[c("plant", "block")], nrow)
block_corners <- function(cells) {
  if (nrow(cells) == 0)
    return(0L)
  neighbours <- rbind(
    cbind(cells[c("row", "col")], dir = "W", row.n = cells$row - 1L, col.n = cells$col),
    cbind(cells[c("row", "col")], dir = "E", row.n = cells$row + 1L, col.n = cells$col),
    cbind(cells[c("row", "col")], dir = "N", row.n = cells$row, col.n = cells$col - 1L),
    cbind(cells[c("row", "col")], dir = "S", row.n = cells$row, col.n = cells$col + 1L),
    cbind(cells[c("row", "col")], dir = "NW", row.n = cells$row - 1L, col.n = cells$col - 1L),
    cbind(cells[c("row", "col")], dir = "NE", row.n = cells$row + 1L, col.n = cells$col - 1L),
    cbind(cells[c("row", "col")], dir = "SW", row.n = cells$row - 1L, col.n = cells$col + 1L),
    cbind(cells[c("row", "col")], dir = "SE", row.n = cells$row + 1L, col.n = cells$col + 1L)
  ) |>
    merge(cells[c("row", "col")], by.x = c("row.n", "col.n"), by.y = c("row", "col")) |>
    merge(cells[c("row", "col")], all.y = TRUE) |>
    subset(, -c(row.n, col.n)) |>
    transform(dir = addNA(dir))
  # corner if two orthogonal neighbours and diagonal neighbour are empty (¬o & ¬d),
  # or (o & ¬d) or (¬o & d), i.e. ¬o | (o & ¬d).
  aggregate(
    neighbours,
    dir ~ row + col,
    \(dirs) {
      (!is.element("N", dirs) && !is.element("E", dirs)) +
        (is.element("N", dirs) && is.element("E", dirs) && !is.element("NE", dirs)) +
        (!is.element("S", dirs) && !is.element("E", dirs)) +
        (is.element("S", dirs) && is.element("E", dirs) && !is.element("SE", dirs)) +
        (!is.element("N", dirs) && !is.element("W", dirs)) +
        (is.element("N", dirs) && is.element("W", dirs) && !is.element("NW", dirs)) +
        (!is.element("S", dirs) && !is.element("W", dirs)) +
        (is.element("S", dirs) && is.element("W", dirs) && !is.element("SW", dirs))
    }
  ) |>
    getElement("dir") |>
    sum()
}
corners <- tapply(
  blocks,
  blocks[c("plant", "block")],
  block_corners
)
sum(area*corners)
