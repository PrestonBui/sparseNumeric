
# sparseNumeric

<!-- badges: start -->

[![R-CMD-check](https://github.com/PrestonBui/sparseNumeric/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PrestonBui/sparseNumeric/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`sparseNumeric` implements an S4 class for sparse numeric vectors and
provides arithmetic, norms, and standardization using the sparse
representation.

\`\`\`r library(sparseNumeric)

v \<- c(0, 4, 3.2, 0, 6.1) x \<- as(v, “sparse_numeric”)

x mean(x) norm(x) xs \<- standardize(x) as(xs, “numeric”)
