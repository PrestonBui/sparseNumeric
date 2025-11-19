setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

setValidity("sparse_numeric", function(object) {
  errs <- character()
  if (!is.numeric(object@value)) errs <- c(errs, "'value' must be numeric.")
  if (!is.integer(object@pos)) errs <- c(errs, "'pos' must be integer.")
  if (!is.integer(object@length) || length(object@length) != 1L)
    errs <- c(errs, "'length' must be a single integer.")
  if (length(object@value) != length(object@pos))
    errs <- c(errs, "'value' and 'pos' must be the same length.")
  if (length(object@length) == 1L && object@length < 0L)
    errs <- c(errs, "'length' must be non-negative.")
  if (length(object@pos)) {
    if (any(object@pos < 1L)) errs <- c(errs, "'pos' must be >= 1.")
    if (any(object@pos > object@length)) errs <- c(errs, "'pos' cannot exceed 'length'.")
    if (any(duplicated(object@pos))) errs <- c(errs, "'pos' must not contain duplicates.")
  }
  if (length(object@value)) {
    if (any(!is.finite(object@value))) errs <- c(errs, "'value' must be finite.")
    if (any(object@value == 0)) errs <- c(errs, "'value' must not contain zeros.")
  }
  if (length(errs)) errs else TRUE
})

setAs("numeric", "sparse_numeric", function(from) {
  n <- length(from)
  if (n == 0L) {
    new("sparse_numeric", value = numeric(), pos = integer(), length = 0L)
  } else {
    nz <- which(from != 0)
    new("sparse_numeric",
        value = as.numeric(from[nz]),
        pos = as.integer(nz),
        length = as.integer(n))
  }
})

setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  if (length(from@pos)) out[from@pos] <- from@value
  out
})

.zero_drop <- function(vals, poss, tol = sqrt(.Machine$double.eps)) {
  keep <- abs(vals) > tol
  list(value = vals[keep], pos = poss[keep])
}

.check_same_length <- function(x, y) {
  if (!is(x, "sparse_numeric") || !is(y, "sparse_numeric"))
    stop("Both arguments must be 'sparse_numeric'.")
  if (x@length != y@length)
    stop("Lengths do not match.")
}

setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))
setGeneric("sparse_norm2", function(x) standardGeneric("sparse_norm2"))
setGeneric("norm", function(x, ...) standardGeneric("norm"))
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

setMethod("sparse_add",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_length(x, y)
            px <- x@pos; py <- y@pos
            vx <- x@value; vy <- y@value
            all_pos <- sort(unique(c(px, py)))
            if (!length(all_pos))
              return(new("sparse_numeric", value = numeric(), pos = integer(), length = x@length))
            ix <- match(all_pos, px, nomatch = 0L)
            iy <- match(all_pos, py, nomatch = 0L)
            vx_u <- numeric(length(all_pos)); nzx <- which(ix != 0L); if (length(nzx)) vx_u[nzx] <- vx[ix[nzx]]
            vy_u <- numeric(length(all_pos)); nzy <- which(iy != 0L); if (length(nzy)) vy_u[nzy] <- vy[iy[nzy]]
            vv <- vx_u + vy_u
            kept <- .zero_drop(vv, all_pos)
            new("sparse_numeric", value = kept$value, pos = as.integer(kept$pos), length = x@length)
          }
)

setMethod("sparse_sub",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_length(x, y)
            px <- x@pos; py <- y@pos
            vx <- x@value; vy <- y@value
            all_pos <- sort(unique(c(px, py)))
            if (!length(all_pos))
              return(new("sparse_numeric", value = numeric(), pos = integer(), length = x@length))
            ix <- match(all_pos, px, nomatch = 0L)
            iy <- match(all_pos, py, nomatch = 0L)
            vx_u <- numeric(length(all_pos)); nzx <- which(ix != 0L); if (length(nzx)) vx_u[nzx] <- vx[ix[nzx]]
            vy_u <- numeric(length(all_pos)); nzy <- which(iy != 0L); if (length(nzy)) vy_u[nzy] <- vy[iy[nzy]]
            vv <- vx_u - vy_u
            kept <- .zero_drop(vv, all_pos)
            new("sparse_numeric", value = kept$value, pos = as.integer(kept$pos), length = x@length)
          }
)

setMethod("sparse_mult",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_length(x, y)
            if (!length(x@pos) || !length(y@pos))
              return(new("sparse_numeric", value = numeric(), pos = integer(), length = x@length))
            common <- intersect(x@pos, y@pos)
            if (!length(common))
              return(new("sparse_numeric", value = numeric(), pos = integer(), length = x@length))
            ix <- match(common, x@pos)
            iy <- match(common, y@pos)
            vv <- x@value[ix] * y@value[iy]
            kept <- .zero_drop(vv, common)
            new("sparse_numeric", value = kept$value, pos = as.integer(kept$pos), length = x@length)
          })

setMethod("sparse_crossprod",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_length(x, y)
            if (!length(x@pos) || !length(y@pos)) return(0)
            common <- intersect(x@pos, y@pos)
            if (!length(common)) return(0)
            ix <- match(common, x@pos)
            iy <- match(common, y@pos)
            sum(x@value[ix] * y@value[iy])
          })

setMethod("+", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))
setMethod("-", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))
setMethod("*", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

setMethod("show", "sparse_numeric", function(object) {
  nnz <- length(object@pos)
  cat("sparse_numeric (length =", object@length, ", nnz =", nnz, ")\n")
  if (nnz == 0L) {
    cat("  <all zeros>\n")
  } else {
    to_show <- min(nnz, 8L)
    cat("  pos  : ", paste(object@pos[seq_len(to_show)], collapse = " "),
        if (nnz > 8L) " ..." else "", "\n", sep = "")
    cat("  value: ", paste(signif(object@value[seq_len(to_show)], 6), collapse = " "),
        if (nnz > 8L) " ..." else "", "\n", sep = "")
  }
})

setMethod("plot",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_length(x, y)
            L <- x@length
            plot(NA, xlim = c(1, max(1L, L)), ylim = range(c(0, x@value, y@value), finite = TRUE),
                 xlab = "Index", ylab = "Value", ...)
            if (length(x@pos)) points(x@pos, x@value)
            if (length(y@pos)) points(y@pos, y@value)
            ov <- intersect(x@pos, y@pos)
            if (length(ov)) {
              ix <- match(ov, x@pos)
              points(ov, x@value[ix])
            }
          })

setMethod("sparse_norm2", "sparse_numeric", function(x) {
  sqrt(sum(x@value^2))
})

setMethod("mean", "sparse_numeric", function(x, ...) {
  if (x@length == 0L) return(NA_real_)
  sum(x@value) / x@length
})

setMethod("norm", "sparse_numeric", function(x, ...) {
  sqrt(sum(x@value^2))
})

setMethod("standardize", "sparse_numeric", function(x, ...) {
  n <- x@length
  if (n == 0L) return(x)
  if (n < 2L) stop("Cannot standardize vector of length < 2.")
  s1 <- sum(x@value)
  s2 <- sum(x@value^2)
  m  <- s1 / n
  var <- (s2 - s1^2 / n) / (n - 1L)
  sd  <- sqrt(var)
  if (!is.finite(sd) || sd == 0) stop("Standard deviation is zero or not finite.")
  if (!length(x@pos)) {
    vals <- rep(-m / sd, n)
    pos  <- seq_len(n)
  } else {
    all_pos  <- seq_len(n)
    zero_pos <- setdiff(all_pos, x@pos)
    vals_nz   <- (x@value - m) / sd
    vals_zero <- rep(-m / sd, length(zero_pos))
    vals <- c(vals_nz, vals_zero)
    pos  <- c(x@pos, zero_pos)
  }
  kept <- .zero_drop(vals, pos)
  new("sparse_numeric",
      value  = kept$value,
      pos    = as.integer(kept$pos),
      length = as.integer(n))
})
