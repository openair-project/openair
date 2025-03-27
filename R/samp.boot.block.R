# TODO: Add comment
#
# Author: tradcc
###############################################################################

samp.boot.block <- function(n, B, blockLength = 20) {
  # Simple block bootstrap, overlapping blocks, no wrap-around,
  # no matching of ends.
  # n = length of data, B = bootstrap replicates
  nblocks <- ceiling(n / blockLength)
  x <- sample(1:(n - blockLength + 1), B * nblocks, replace = TRUE)
  dim(x) <- c(nblocks, B)
  apply(x, 2, function(y, L) (0:(L - 1)) + rep(y, each = L), L = blockLength)[
    1:n,
  ]
}
