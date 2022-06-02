library(sloop)

# A base object:
is.object(1:10)
#> [1] FALSE
sloop::otype(1:10)
#> [1] "base"

# An OO object
is.object(mtcars)
#> [1] TRUE
sloop::otype(mtcars)
#> [1] "S3"

# Technically, the difference between base and OO objects is that OO objects have a “class” attribute:

attr(1:10, "class")
#> NULL

attr(mtcars, "class")
#> [1] "data.frame"

s3_class(1:10)
s3_class(mtcars)

class(1:10)
class(mtcars)


### S3

### 13.2 Basics

f <- factor(c("a", "b", "c"))

typeof(f)
#> [1] "integer"
attributes(f)
#> $levels
#> [1] "a" "b" "c"
#> 
#> $class
#> [1] "factor"

unclass(f)
#> [1] 1 2 3
#> attr(,"levels")
#> [1] "a" "b" "c"

ftype(print)
#> [1] "S3"      "generic"
ftype(str)
#> [1] "S3"      "generic"
ftype(unclass)
#> [1] "primitive"


print(f)
#> [1] a b c
#> Levels: a b c

# stripping class reverts to integer behavior
print(unclass(f))
#> [1] 1 2 3
#> attr(,"levels")
#> [1] "a" "b" "c"

# Beware that str() is generic, and some S3 classes use that generic to hide the internal details.
# For example, the POSIXlt class used to represent date-time data is actually built on top of a list, a fact which is hidden by its str() method:

time <- strptime(c("2017-01-01", "2020-05-04 03:21"), "%Y-%m-%d")
str(time)
#>  POSIXlt[1:2], format: "2017-01-01" "2020-05-04"

str(unclass(time))
#> List of 9
#>  $ sec  : num [1:2] 0 0
#>  $ min  : int [1:2] 0 0
#>  $ hour : int [1:2] 0 0
#>  $ mday : int [1:2] 1 4
#>  $ mon  : int [1:2] 0 4
#>  $ year : int [1:2] 117 120
#>  $ wday : int [1:2] 0 1
#>  $ yday : int [1:2] 0 124
#>  $ isdst: int [1:2] 0 0
#>  - attr(*, "tzone")= chr "UTC"

### The implementation for a specific class is called a method, and the generic finds that method by performing method dispatch. ###

s3_dispatch(print(f))

# Unlike most functions, you can’t see the source code for most S3 methods

weighted.mean.Date
#> Error in eval(expr, envir, enclos): object 'weighted.mean.Date' not found

s3_get_method(weighted.mean.Date)
#> function (x, w, ...) 
#> .Date(weighted.mean(unclass(x), w, ...))
#> <bytecode: 0x7f9682f700b8>
#> <environment: namespace:stats>

### EXERCISE ###

x <- ecdf(rpois(100, 10))
x
#> Empirical CDF 
#> Call: ecdf(rpois(100, 10))
#>  x[1:18] =  2,  3,  4,  ..., 2e+01, 2e+01


typeof(x)
#> [1] "closure"

attributes(x)
#> $class
#> [1] "ecdf"     "stepfun"  "function"
#> 
#> $call
#> ecdf(rpois(100, 10))
