library(lobstr)
library(rlang)
### TRACEBACK() !!! ###

f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) {
  if (!is.numeric(d)) {
    lobstr::cst()
    stop("`d` must be numeric", call. = FALSE)
  }
  d + 10
}

f("a")

2## 22.3.1 Lazy evaluation
j <- function() k()
k <- function() stop("Oops!", call. = FALSE)
f(j())
#> Error: Oops!

rlang::with_abort(f(j()))
rlang::last_trace()
