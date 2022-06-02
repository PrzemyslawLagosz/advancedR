library(sloop)

#  S3 has no formal definition of a class: to make an object an instance of a class, you simply set the class attribute

# Create and assign class in one step
x <- structure(list(), class = "my_class")

# Create, then set class
x <- list()
class(x) <- "my_class"

class(x)
#> [1] "my_class"
inherits(x, "my_class")
#> [1] TRUE
inherits(x, "your_class")
#> [1] FALSE

# To avoid foot-bullet intersections when creating your own class, I recommend that you usually provide three functions:

# A low-level constructor, new_myclass(), that efficiently creates new objects with the correct structure.
# A validator, validate_myclass(), that performs more computationally expensive checks to ensure that the object has correct values.
# A user-friendly helper, myclass(), that provides a convenient way for others to create objects of your class.

## 13.3.1 Constructors

# The constructor should follow three principles:

# Be called new_myclass().
# Have one argument for the base object, and one for each attribute.
# Check the type of the base object and the types of each attribute.

new_Date <- function(x = double()) {
  stopifnot(is.double(x))
  structure(x, class = "Date")
}

new_Date(1000)

new_difftime <- function(x = double(), units = "secs") {
  stopifnot(is.double(x))
  units <- match.arg(units, c("secs", "mins", "hours", "days", "weeks"))
  
  structure(
    x,
    class = "difftime",
    units = units
  )
}

new_difftime(10, "weeks")

attributes(new_difftime)


## 13.3.2 Validators

new_factor <- function(x = integer(), levels = character()) {
  stopifnot(is.integer(x))
  stopifnot(is.character(levels))
  
  structure(
    x,
    levels = levels,
    class = "factor"
  )
}

new_factor(1:5, "a")
#> Error in as.character.factor(x): malformed factor
new_factor(0:1, "a")
#> Error in as.character.factor(x): malformed factor

validate_factor <- function(x) {
  values <- unclass(x) # traktuje factor jak zwykły wektor
  levels <- attr(x, "levels") # zapisuje poziomy jako zwykły wektor string
  
  if (!all(!is.na(values) & values > 0)) {
    stop(
      "All `x` values must be non-missing and greater than zero",
      call. = FALSE
    )
  }
  
  if (length(levels) < max(values)) {
    stop(
      "There must be at least as many `levels` as possible values in `x`",
      call. = FALSE
    )
  }
  
  x
}

levels <- attr(f, "levels")

validate_factor(new_factor(0:5, "a"))

## 13.3.3 Helpers
m
# A helper should always:
#   
# Have the same name as the class, e.g. myclass().
# Finish by calling the constructor, and the validator, if it exists.
# Create carefully crafted error messages tailored towards an end-user.
# Have a thoughtfully crafted user interface with carefully chosen default values and useful conversions.

new_difftime(1:10)
#> Error in new_difftime(1:10): is.double(x) is not TRUE

difftime <- function(x = double(), units = "secs") {
  x <- as.double(x) # It’s not the job of the constructor to be flexible, so here we create a helper that just coerces the input to a double.
  new_difftime(x, units = units)
}

difftime(1:10)
#> Time differences in secs
#>  [1]  1  2  3  4  5  6  7  8  9 10



factor <- function(x = character(), levels = unique(x)) {
  ind <- match(x, levels)
  validate_factor(new_factor(ind, levels))
}

factor(c("a", "a", "b"))
#> [1] a a b
#> Levels: a b

### EXERCISE ###

# 1.
new_data.frame <- function(x, n, row.names = NULL) {
  stopifnot(is.list(x))
  stopifnot(all(length(x) == n))
  
  if (is.null(row.names)) {
    row.names <- .set_row_names(n)
  } else {
    stopifnot(is.character(row.names), length(row.names) == n)
  }
  
  structure(
    x,
    class = "data.frame",
    row.names = row.names
  )
}

# Test
x <- list(a = 1, b = 2)
new_data.frame(x, n = 1)
#>   a b
#> 1 1 2
new_data.frame(x, n = 1, row.names = "l1")
#>    a b
#> l1 1 2


# 2.

factor2 <- function(x, levels = unique(x)) {
  new_levels <- match(x, levels)
  
  missing <- unique(setdiff(x, levels))
  
  if (length(missing) > 0) {
    stop(
      "The following values do not occur in the levels of x: ",
      paste0("'", missing, "'"), ".",
      call. = FALSE
    )
  }
  
  validate_factor(new_factor(new_levels, levels))
}

factor2(c("a", "a", "b"))
factor2(c("a", "b", "c"), levels = c("a", "b"))


# 5.
# Constructor
new_roman <- function(x = integer()) {
  stopifnot(is.integer(x))
  structure(
    x,
    class = "roman"
  )
}

#Validator
validate_roman <- function(x) {
  values <- unclass(x)
  
  if(any(values < 1 | values > 3899)) {
    stop(
      "Roman numbers must fall between 1 and 3899.",
      call. = FALSE
    )
  }
  
  x
}

#Helper
roman <- function(x = integer()) {
  x <- as.integer(x)
  
  validate_roman(new_roman(x))
}

roman(7)



### 13.4 Generics and methods
# https://adv-r.hadley.nz/s3.html#s3-methods

# The job of an S3 generic is to perform method dispatch, i.e. find the specific implementation for a class. 

g <- function(x) {
  x <- 10
  y <- 10
  UseMethod("g")
}
g.default <- function(x) c(x = x, y = y)

x <- 2
y <- 2
g(x)
#>  x  y 
#>  1 10



### 13.6 Inheritance

## 13.6.1 NextMethod()

new_secret <- function(x = double()) {
  stopifnot(is.double(x))
  structure(x, class = "secret")
}

print.secret <- function(x, ...) {
  print(strrep("x", nchar(x)))
  invisible(x)
}

# This works, but is inefficient because it creates a copy of x
`[.secret` <- function(x, i) {
  x <- unclass(x)
  new_secret(x[i])
}

`[.secret` <- function(x, i) {
  new_secret(NextMethod())
}

x[1]
#> [1] "xx"

gg <- new_secret(c(15, 124))

#The => indicates that [.secret is called, but that NextMethod() delegates work to the underlying internal [ method, as shown by the ->.

s3_dispatch(gg[1])
#> => [.secret
#>    [.default
#> -> [ (internal)


## 13.6.2 Allowing subclassing

# To allow subclasses, the parent constructor needs to have ... and class arguments:

# Parent constructor
new_secret <- function(x, ..., class=character()) {
  stopifnot(is.double(x))
  
  structure(
    x,
    ...,
    class = c(class, "secret")
  )
}

# Subclass constructor

new_supersecret <- function(x) {
  new_secret(x, class = "supersecret")
}

print.supersecret <- function(x, ...) {
  print(rep("xxxx", length(x)))
  invisible(x)
}

x2 <- new_supersecret(c(1, 123))
x2

x2[2] # <- zachowanie jak klasa secret nie supersecret


### StackOverflow ###
x <- 1
attr(x,'class') <- c('first','second')
