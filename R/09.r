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
    values = head(as.vector(rbind(seq_along(block_sizes) - 1L, NA_integer_)), length(x))
  ),
  class = "rle"
)

while (any(is.na(enc$values))) {
  hole_pos <- match(NA_integer_, enc$values)
  block_pos <- Position(Negate(is.na), enc$values, right = TRUE)
  hole_len <- enc$lengths[[hole_pos]]
  block_len <- enc$lengths[[block_pos]]
  if (block_len < hole_len) {
    enc$lengths[[hole_pos]] <- hole_len - block_len
    enc$values <- append(enc$values, enc$values[[block_pos]], after = hole_pos - 1L)
    enc$lengths <- append(enc$lengths, block_len, after = hole_pos - 1L)
    enc$lengths[[block_pos]] <- 0L
  }else{
    enc$values[[hole_pos]] <- enc$values[[block_pos]]
    enc$lengths[[block_pos]] <- block_len - hole_len
  }
  if (enc$lengths[[block_pos]] == 0) {
    enc$values <- head(enc$values, -1)
    enc$lengths <- head(enc$lengths, -1)
    if (is.na(tail(enc$values, 1))) {
      enc$values <- head(enc$values, -1)
      enc$lengths <- head(enc$lengths, -1)
    }
  }
}

# part one: 6201130364722

memblock <- inverse.rle(enc)
sum(memblock*(seq_along(memblock) - 1L))
