x <- readLines("inputs/05.txt")
gap <- which(x == "")
pords <- x[seq_len(gap - 1)] |>
  strsplit("|", fixed = TRUE) |>
  lapply(as.integer) |>
  do.call(what = rbind)
updates <- x[seq(gap + 1, length(x))] |>
  strsplit(",", fixed = TRUE) |>
  lapply(as.integer)

# part one
pord_ints <- sort(unique(as.vector(pords)))
pord_indices <- pords
pord_indices[] <- match(pord_indices, pord_ints)
pord_mat <- matrix(FALSE, nrow = length(pord_ints), ncol = length(pord_ints))
for (n in seq_len(nrow(pord_indices))) {
  pord_mat[[
    pord_indices[[n, 1]],
    pord_indices[[n, 2]]
  ]] <- TRUE
}
update_indices <- lapply(updates, match, pord_ints)
sorted <- vapply(
  update_indices,
  \(x) {
    submat <- pord_mat[x, x]
    all(!submat[lower.tri(submat, diag = FALSE)])
  },
  logical(1)
)
sum(vapply(updates[sorted], \(x) x[(length(x) + 1)/2], integer(1)))

# part two
unsorted_ords <- lapply(
  update_indices[!sorted],
  \(x) {
    submat <- pord_mat[x, x]
    rs <- rowSums(submat)
    stopifnot(setequal(rs, seq_along(x) - 1L))
    order(rs)
  }
)
sorted_nonsorted <- Map(`[`, updates[!sorted], unsorted_ords)
sum(vapply(sorted_nonsorted, \(x) x[(length(x) + 1)/2], integer(1)))
