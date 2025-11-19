\name{sparse_arith-operators}
\alias{+,sparse_numeric,sparse_numeric-method}
\alias{-,sparse_numeric,sparse_numeric-method}
\alias{*,sparse_numeric,sparse_numeric-method}
\title{Arithmetic operators for sparse_numeric vectors}
\usage{
\S4method{+}{sparse_numeric,sparse_numeric}(e1, e2)
\S4method{-}{sparse_numeric,sparse_numeric}(e1, e2)
\S4method{*}{sparse_numeric,sparse_numeric}(e1, e2)
}
\arguments{
  \item{e1}{A \code{sparse_numeric} object.}
  \item{e2}{A \code{sparse_numeric} object.}
}
\value{
A \code{sparse_numeric} object containing the element-wise result of
applying the operator.
}
\description{
Methods for the \code{+}, \code{-}, and \code{*} operators for
\code{sparse_numeric} vectors. These methods perform arithmetic directly
on the sparse representation.
}
