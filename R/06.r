x <- readLines("inputs/06.txt") |>
  strsplit("", fixed = TRUE) |>
  do.call(what = rbind)
size <- dim(x)
start <- as.vector(which(x == "^", arr.ind = TRUE))
obstacles <- as.data.frame(which(x == "#", arr.ind = TRUE))

# part one: 5318
jump <- function(pos, direction = c("up", "down", "left", "right")) {
  direction <- match.arg(direction)
  switch(
    direction,
    up = {
      hit <- obstacles |>
        subset(row < pos[1] & col == pos[2]) |>
        getElement("row") |>
        max() |>
        suppressWarnings()
      if (is.infinite(hit))
        list(
          c(NA_integer_, pos[2]),
          data.frame(row = seq_len(pos[1]), col = pos[2])
        )
      else
        list(
          c(hit + 1L, pos[2]),
          data.frame(row = seq(pos[1], hit + 1L), col = pos[2])
        )
    },
    down = {
      hit <- obstacles |>
        subset(row > pos[1] & col == pos[2]) |>
        getElement("row") |>
        min() |>
        suppressWarnings()
      if (is.infinite(hit))
        list(
          c(NA_integer_, pos[2]),
          data.frame(row = seq(pos[1], size[1]), col = pos[2])
        )
      else
        list(
          c(hit - 1L, pos[2]),
          data.frame(row = seq(pos[1], hit - 1L), col = pos[2])
        )
    },
    left = {
      hit <- obstacles |>
        subset(row == pos[1] & col < pos[2]) |>
        getElement("col") |>
        max() |>
        suppressWarnings()
      if (is.infinite(hit))
        list(
          c(pos[1], NA_integer_),
          data.frame(row = pos[1], col = seq_len(pos[2]))
        )
      else
        list(
          c(pos[1], hit + 1L),
          data.frame(row = pos[1], col = seq(pos[2], hit + 1L))
        )
    },
    right = {
      hit <- obstacles |>
        subset(row == pos[1] & col > pos[2]) |>
        getElement("col") |>
        min() |>
        suppressWarnings()
      if (is.infinite(hit))
        list(
          c(pos[1], NA_integer_),
          data.frame(row = pos[1], col = seq(pos[2], size[2]))
        )
      else
        list(
          c(pos[1], hit - 1L),
          data.frame(row = pos[1], col = seq(pos[2], hit - 1L))
        )
    }
  )
}

pos <- start
direction <- "up"
visited <- data.frame(row = pos[1], col = pos[2])

while (!anyNA(pos)) {
  j <- jump(pos, direction)
  pos <- j[[1]]
  visited <- rbind(visited, j[[2]])
  direction <- c("right", "down", "left", "up")[
    match(direction, c("up", "right", "down", "left"))
  ]
}

nrow(unique(visited))
