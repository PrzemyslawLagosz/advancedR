library(profvis)
library(bench)

### 23.2 Profiling

f <- function() {
  pause(0.1)
  g()
  h()
}
g <- function() {
  pause(0.1)
  h()
}
h <- function() {
  pause(0.1)
}

f()

tmp <- tempfile()
Rprof(tmp, interval = 0.1)
f()
Rprof(NULL)
writeLines(readLines(tmp))

#> sample.interval=100000
#> "pause" "f" 
#> "pause" "g" "f" 
#> "pause" "h" "g" "f" 
#> "pause" "h" "f" 

# Each line represents one “tick” of the profiler (0.1 s in this case), and function calls are recorded 
# from right to left: the first line shows f() calling pause(). It shows that the code spends 0.1 s running f(), 
# then 0.2 s running g(), then 0.1 s running h().

## 23.2.1 Visualising profiles
# The default profiling resolution is quite small, so if your function takes even a few seconds it will generate hundreds of samples.

profvis(f())

profvis({
x <- integer()
for (i in 1:1e4) {
  x <- c(x, i)
}
})

profvis({
f <- function(n = 1e5) {
  x <- rep(1, n)
  rm(x)
}
}, torture = TRUE)


### 23.3 Microbenchmarking
# A microbenchmark is a measurement of the performance of a very small piece of code, something that might take
# milliseconds (ms), microseconds (µs), or nanoseconds (ns) to run

x <- runif(100)
(lb <- bench::mark(
  sqrt(x),
  x ^ 0.5
))
#> # A tibble: 2 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 sqrt(x)       865ns   1.05µs   679021.      848B        0
#> 2 x^0.5        3.78µs   4.17µs   203205.      848B        0

plot(lb)
#> Loading required namespace: tidyr