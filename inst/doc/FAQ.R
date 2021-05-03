## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(simpleMH)

## -----------------------------------------------------------------------------
p.log.restricted <- function(x) {
  
  if (any(x < 0, x > 1)) {
    return(-Inf)
  }
  
  B <- 0.03 # controls 'bananacity'
  -x[1]^2 / 200 - 1 / 2 * (x[2] + B * x[1]^2 - 100 * B)^2
}

res <- simpleMH(
  p.log.restricted,
  inits = c(a = 0, b = 0),
  theta.cov = diag(2),
  max.iter = 3000,
  coda = TRUE
)
summary(res$samples)

## ---- eval = identical(Sys.getenv("IN_PKGDOWN"), "true")----------------------
#  plot(res$samples)

