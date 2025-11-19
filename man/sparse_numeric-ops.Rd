\name{sparse_numeric-ops}
\alias{sparse_add}
\alias{sparse_sub}
\alias{sparse_mult}
\alias{sparse_crossprod}
\alias{sparse_norm2}
\alias{standardize}
\alias{norm}
\alias{norm,sparse_numeric-method}
\alias{sparse_add,sparse_numeric,sparse_numeric-method}
\alias{sparse_sub,sparse_numeric,sparse_numeric-method}
\alias{sparse_mult,sparse_numeric,sparse_numeric-method}
\alias{sparse_crossprod,sparse_numeric,sparse_numeric-method}
\alias{sparse_norm2,sparse_numeric-method}
\alias{standardize,sparse_numeric-method}
\title{Operations for \code{sparse_numeric} vectors}
\description{
Functions and methods for arithmetic and related operations on
\code{sparse_numeric} vectors.
}
\usage{
sparse_add(x, y, ...)
sparse_sub(x, y, ...)
sparse_mult(x, y, ...)
sparse_crossprod(x, y, ...)
sparse_norm2(x)
standardize(x, ...)
}
\arguments{
  \item{x, y}{Objects of class \code{sparse_numeric}.}
  \item{\dots}{Further arguments passed to methods (currently unused).}
}
\value{
For \code{sparse_add}, \code{sparse_sub}, and \code{sparse_mult}, a
\code{sparse_numeric} object representing the element-wise result.

For \code{sparse_crossprod}, a numeric scalar giving the inner product.

For \code{sparse_norm2}, a numeric scalar giving the Euclidean norm.

For \code{standardize}, a \code{sparse_numeric} object containing the
standardized vector (mean 0 and standard deviation 1).
}
\details{
These functions operate directly on the sparse representation and do not
materialize the full dense vector in memory.
}
\seealso{
\code{\link{sparse_numeric-class}} for a description of the class.
}
