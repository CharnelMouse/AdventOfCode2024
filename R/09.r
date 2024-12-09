x <- readLines("inputs/09.txt") |>
  strsplit("", fixed = TRUE) |>
  getElement(1) |>
  as.integer()
len <- length(x)
filled <- as.logical(seq_along(x) %% 2L)
block_sizes <- x[filled]
gap_sizes <- x[!filled]

enc <- structure(
  list(
    lengths = x,
    values = head(as.vector(rbind(seq_along(block_sizes) - 1L, -1L)), length(x))
  ),
  class = "rle"
)

# part one: 6201130364722

mem_rle <- enc

while (any(mem_rle$values == -1)) {
  hole_pos <- match(-1L, mem_rle$values)
  block_pos <- Position(\(x) x != -1L, mem_rle$values, right = TRUE)
  hole_len <- mem_rle$lengths[[hole_pos]]
  block_len <- mem_rle$lengths[[block_pos]]
  if (block_len < hole_len) {
    mem_rle$lengths[[hole_pos]] <- hole_len - block_len
    mem_rle$lengths[[block_pos]] <- 0L
    mem_rle$values <- append(mem_rle$values, mem_rle$values[[block_pos]], after = hole_pos - 1L)
    mem_rle$lengths <- append(mem_rle$lengths, block_len, after = hole_pos - 1L)
  }else{
    mem_rle$values[[hole_pos]] <- mem_rle$values[[block_pos]]
    mem_rle$lengths[[block_pos]] <- block_len - hole_len
  }
  mem_rle <- rle(inverse.rle(mem_rle))
}

memblock <- inverse.rle(mem_rle)
memblock[memblock == -1] <- 0L
sum(memblock*(seq_along(memblock) - 1L))

# part two: 6221662795602

mem_rle2 <- enc

for (id in rev(seq_len(max(enc$values)))) {
  hole_poses <- which(mem_rle2$values == -1)
  hole_sizes <- mem_rle2$lengths[hole_poses]
  max_hole_size <- max(hole_sizes)
  block_pos <- which(mem_rle2$values == id)
  block_len <- mem_rle2$lengths[[block_pos]]
  insertion <- match(TRUE, hole_sizes >= block_len)
  if (is.na(insertion))
    next
  hole_pos <- hole_poses[insertion]
  if (hole_pos > block_pos)
    next
  # print(paste("move", id))
  hole_len <- mem_rle2$lengths[[hole_pos]]
  mem_rle2$lengths[[hole_pos]] <- hole_len - block_len
  mem_rle2$values[[block_pos]] <- -1L
  mem_rle2$values <- append(mem_rle2$values, id, after = hole_pos - 1L)
  mem_rle2$lengths <- append(mem_rle2$lengths, block_len, after = hole_pos - 1L)
  while (tail(mem_rle2$values, 1) == -1) {
    mem_rle2$lengths <- head(mem_rle2$lengths, -1)
    mem_rle2$values <- head(mem_rle2$values, -1)
  }
  mem_rle2 <- rle(inverse.rle(mem_rle2))
  # print(inverse.rle(mem_rle2))
  hole_poses <- which(mem_rle2$values == -1)
  hole_sizes <- mem_rle2$lengths[hole_poses]
  max_hole_size <- max(hole_sizes)
  moveable_blocks <- which(mem_rle2$values != -1 & mem_rle2$lengths <= max_hole_size)
}

memblock2 <- inverse.rle(mem_rle2)
memblock2[memblock2 == -1] <- 0L
sum(memblock2*(seq_along(memblock2) - 1L), na.rm = TRUE)
