\name{sparse_numeric-class}
\docType{class}
\alias{sparse_numeric-class}
\alias{sparse_numeric}
\title{Sparse numeric vector class}
\description{
The \code{sparse_numeric} S4 class stores a numeric vector in sparse
form, keeping only the non-zero values and their positions.
}
\section{Slots}{
  \describe{
    \item{\code{value}}{Numeric vector of non-zero values.}
    \item{\code{pos}}{Integer vector of 1-based positions of the non-zero values.}
    \item{\code{length}}{Single integer giving the total length of the
    full (dense) vector.}
  }
}
\seealso{
\code{\link{sparse_add}}, \code{\link{sparse_sub}},
\code{\link{sparse_mult}}, \code{\link{sparse_crossprod}},
\code{\link{sparse_norm2}}, \code{\link{standardize}}.
}
