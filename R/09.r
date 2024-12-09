# RLE is a compact way to track memory blocks, but requires some manual merging
# of adjacent blocks to avoid repeatedly inverting and re-applying RLE

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
  while (tail(mem_rle$lengths, 1) == 0 || tail(mem_rle$values, 1) == -1) {
    mem_rle$lengths <- head(mem_rle$lengths, -1)
    mem_rle$values <- head(mem_rle$values, -1)
  }
}

memblock <- inverse.rle(mem_rle)
memblock[memblock == -1] <- 0L
format(sum(memblock*(seq_along(memblock) - 1L)), scientific = FALSE)

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
  hole_len <- mem_rle2$lengths[[hole_pos]]
  # move copy of block
  mem_rle2$lengths[[hole_pos]] <- hole_len - block_len
  mem_rle2$values[[block_pos]] <- -1L
  mem_rle2$lengths <- append(mem_rle2$lengths, block_len, after = hole_pos - 1L)
  mem_rle2$values <- append(mem_rle2$values, id, after = hole_pos - 1L)
  # merge blocks on either side of old block if same value (i.e. empty)
  # old block with right
  if (
    block_pos + 1 < length(mem_rle2$lengths) &&
    mem_rle2$values[[block_pos + 1]] == mem_rle2$values[[block_pos + 2]]
  ) {
    mem_rle2$lengths[[block_pos + 1]] <- mem_rle2$lengths[[block_pos + 1]] + mem_rle2$lengths[[block_pos + 2]]
    mem_rle2$lengths <- mem_rle2$lengths[-(block_pos + 2)]
    mem_rle2$values <- mem_rle2$values[-(block_pos + 2)]
  }
  # old block with left
  if (
    block_pos < length(mem_rle2$lengths) &&
    mem_rle2$values[[block_pos]] == mem_rle2$values[[block_pos + 1]]
  ) {
    mem_rle2$lengths[[block_pos]] <- mem_rle2$lengths[[block_pos]] + mem_rle2$lengths[[block_pos + 1]]
    mem_rle2$lengths <- mem_rle2$lengths[-(block_pos + 1)]
    mem_rle2$values <- mem_rle2$values[-(block_pos + 1)]
  }
  zero_width <- mem_rle2$lengths == 0
  mem_rle2$lengths <- mem_rle2$lengths[!zero_width]
  mem_rle2$values <- mem_rle2$values[!zero_width]
}

memblock2 <- inverse.rle(mem_rle2)
memblock2[memblock2 == -1] <- 0L
format(sum(memblock2*(seq_along(memblock2) - 1L), na.rm = TRUE), scientific = FALSE)
