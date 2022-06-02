library(purrr)

### <- Main topics
## <- Sub topics

# A functional is a function that takes a function as an input and returns a vector as output.

randomise <- function(f) f(runif(1e3))
randomise(mean)
#> [1] 0.506
randomise(mean)
#> [1] 0.501
randomise(sum)
#> [1] 489

### 9.2 My first functional: map()

# It takes a vector and a function, calls the function once for each element of the vector, and returns the results in a list.
triple <- function(x) x * 3
map(1:3, triple)

simple_map <- function(x, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], ...)
  }
  out
}

# map_chr() always returns a character vector
map_chr(mtcars, typeof)
#>      mpg      cyl     disp       hp     drat       wt     qsec       vs 
#> "double" "double" "double" "double" "double" "double" "double" "double" 
#>       am     gear     carb 
#> "double" "double" "double"

# map_lgl() always returns a logical vector
map_lgl(mtcars, is.double)
#>  mpg  cyl disp   hp drat   wt qsec   vs   am gear carb 
#> TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

# map_int() always returns a integer vector
n_unique <- function(x) length(unique(x))
map_int(mtcars, n_unique)
#>  mpg  cyl disp   hp drat   wt qsec   vs   am gear carb 
#>   25    3   27   22   22   29   30    2    2    3    6

# map_dbl() always returns a double vector
map_dbl(mtcars, mean)
#>     mpg     cyl    disp      hp    drat      wt    qsec      vs      am    gear 
#>  20.091   6.188 230.722 146.688   3.597   3.217  17.849   0.438   0.406   3.688 
#>    carb 
#>   2.812


## 9.2.2 Anonymous functions and shortcuts

# The function arguments look a little quirky but allow you to refer to . for one argument functions, .x and .y for two 
# argument functions, and ..1, ..2, ..3, etc, for functions with an arbitrary number of arguments. 

map_dbl(mtcars, function(x) length(unique(x)))
#>  mpg  cyl disp   hp drat   wt qsec   vs   am gear carb 
#>   25    3   27   22   22   29   30    2    2    3    6

map_dbl(mtcars, ~ length(unique(.x)))
#>  mpg  cyl disp   hp drat   wt qsec   vs   am gear carb 
#>   25    3   27   22   22   29   30    2    2    3    6

map_dbl(1:3, function(x) log(x, base = 2))
map_dbl(1:3, ~ log(.x, base = 2))


x <- map(1:3, ~ runif(2))
str(x)
#> List of 3
#>  $ : num [1:2] 0.281 0.53
#>  $ : num [1:2] 0.433 0.917
#>  $ : num [1:2] 0.0275 0.8249


x <- list(
  list(-1, x = 1, y = c(2), z = "a"),
  list(-2, x = 4, y = c(5, 6), z = "b"),
  list(-3, x = 8, y = c(9, 10, 11))
)

# Select by name
map_dbl(x, "x")
#> [1] 1 4 8

# Or by position
map_dbl(x, 1)
#> [1] -1 -2 -3

# Or by both
map_dbl(x, list("y", 1))
#> [1] 2 5 9

# You'll get an error if a component doesn't exist:
map_chr(x, "z")
#> Error: Result 3 must be a single string, not NULL of length 0

# Unless you supply a .default value
map_chr(x, "z", .default = NA)
#> [1] "a" "b" NA



## 9.2.3 Passing arguments with ...

x <- list(1:5, c(1:10, NA))

map_dbl(x, ~ mean(.x, na.rm = TRUE))
#> [1] 3.0 5.5

map_dbl(x, mean, na.rm = TRUE)
#> [1] 3.0 5.5

plus <- function(x, y) x + y

# Extra arguments in an anonymous function means that they will be evaluated every time f() is executed, not just once when you call map(). !

x <- c(0, 0, 0, 0)
map_dbl(x, plus, runif(1))
#> [1] 0.0625 0.0625 0.0625 0.0625
map_dbl(x, ~ plus(.x, runif(1)))
#> [1] 0.903 0.132 0.629 0.945



## 9.2.5 Varying another argument

trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)

map_dbl(trims, function(trim) mean(x, trim = trim))



### EXERCISE ###

as_mapper(c(1, 2))  # equivalent to function(x) x[[1]][[2]]
as_mapper(c("a", "b"))  # equivalent to function(x) x[["a"]][["b]]
as_mapper(list(1, "b"))  # equivalent to function(x) x[[1]][["b]]

# Define custom accessor function
get_class <- function(x) attr(x, "class")
pluck(mtcars, get_class)
#> [1] "data.frame"

# Use attr_getter() as a helper
pluck(mtcars, attr_getter("class"))
#> [1] "data.frame"


# Q3: Use the appropriate map() function to:
#   
# Compute the standard deviation of every column in a numeric data frame.
 
map_dbl(mtcars, sd)

# Compute the standard deviation of every numeric column in a mixed data frame. (Hint: you’ll need to do it in two steps.)

mtcars_numeric <- map_lgl(mtcars, is.numeric)
map_dbl(mtcars[ ,mtcars_numeric], sd, na.rm=TRUE)

# Compute the number of levels for every factor in a data frame.

penguins <- palmerpenguins::penguins

penguins_factor <- map_lgl(penguins, is.factor)
map_int(penguins[penguins_factor], ~ length(levels(.x)))
#> species  island     sex 
#>       3       3       2

# Q4: The following code simulates the performance of a t-test for non-normal data. Extract the p-value from each test, then visualise.
# A two sample t-test is used to determine whether or not two population means are equal.
# https://www.statology.org/two-sample-t-test/

library(ggplot2)
trials <- map(1:100, ~ t.test(rpois(10, 10), rpois(10, 7)))

df_trials <- tibble::tibble(p_value = map_dbl(trials, "p.value"))

ggplot(data = df_trials, aes(x = p_value, fill = p_value < 0.05)) +
  geom_dotplot() +
  theme(
    legend.position = "top"
  )

# Q5
x <- list(
  list(1, c(3, 9)),
  list(c(3, 6), 7, c(4, 7, 6))
)

triple <- function(x) x * 3
map(x, map, .f = triple)
#> Error in .f(.x[[i]], ...): unused argument (function (.x, .f, ...) 
#> {
#>     .f <- as_mapper(.f, ...)
#>     .Call(map_impl, environment(), ".x", ".f", "list")
#> })

# Don't name the argument
map(x, map, triple)

# Use magrittr-style anonymous function
map(x, . %>% map(triple))

# Use purrr-style anonymous function
map(x, ~ map(.x, triple))

# Q6: Use map() to fit linear models to the mtcars dataset using the formulas stored in this list:

formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

models <- map(formulas, lm, data = mtcars)

# Q7: Fit the model mpg ~ disp to each of the bootstrap replicates of mtcars in the list below, then extract the R2 of the model fit (Hint: you can compute the R2 with summary())

bootstrap <- function(df) {
  df[sample(nrow(df), replace = TRUE), , drop = FALSE]
}

bootstraps <- map(1:10, ~ bootstrap(mtcars))

bootstraps %>%
  map(~ lm(mpg ~ disp, data = .x)) %>%
  map(summary) %>%
  map_dbl("r.squared")
#>  [1] 0.588 0.822 0.745 0.746 0.784 0.749 0.613 0.792 0.653 0.726

### 9.3 Purrr style

by_cyl <- split(mtcars, mtcars$cyl)

by_cyl %>% 
  map(~ lm(mpg ~ wt, data = .x)) %>% 
  map(coef)

# $`4`
# (Intercept)          wt 
# 39.571196   -5.647025 
# 
# $`6`
# (Intercept)          wt 
# 28.408845   -2.780106 
# 
# $`8`
# (Intercept)          wt 
# 23.868029   -2.192438 

by_cyl %>% 
  map(~ lm(mpg ~ wt, data = .x)) %>%  # <- Wyznacza model liniowy dla kazdego elementu listy
  map(coef) %>%                       # <- Wybiera z każdego elementu listy który zawiera formułe liniową tylko `coef` element
  map_dbl(2)                          # <- Wybiera z każdego elementu listy tylko 2 element
#>     4     6     8 
#> -5.65 -2.78 -2.19



### 9.4 Map variants

## 9.4.1 Same type of output as input: modify()

df <- data.frame(
  x = 1:3,
  y = 6:4
)

map(df, function(x) x * 2)
map(df, ~ .x * 2)

modify(df, function(x) x * 2) # df IN -- df OUT
modify(df, 1) # zmienia wszyskie wiersze na wartosci jak w pierwszyme

simple_modify <- function(x, f, ...) {
  for (i in seq_along(x)) {
    x[[i]] <- f(x[[i]], ...)
  }
  x
}

## 9.4.2 Two inputs: map2() and friends

xs <- map(1:8, ~ runif(10))
xs[[1]][[1]] <- NA
ws <- map(1:8, ~ rpois(10, 5) + 1)

# You can use map_dbl() to compute the unweighted means:
map_dbl(xs, mean)

weighted.mean(x = c(1:3), w = c(10,1,1)) # <- x =wektor ocenek, w = wektor wagi kazdej ocenki

# But passing ws as an additional argument doesn’t work because arguments after .f are not transformed:
map_dbl(xs, weighted.mean, w = ws)

# We need a new tool: a map2(), which is vectorised over two arguments. This means both .x and .y are varied in each call to .f:
map2_dbl(xs, ws, weighted.mean)
#> [1]    NA 0.451 0.603 0.452 0.563 0.510 0.342 0.464

## 9.4.3 No outputs: walk() and friends

welcome <- function(x) {
  cat("Welcome ", x, "!\n", sep = "")
}
names <- c("Hadley", "Jenny")

# As well as generate the welcomes, it also shows 
# the return value of cat()
map(names, welcome)
#> Welcome Hadley!
#> Welcome Jenny!
#> [[1]]
#> NULL
#> 
#> [[2]]
#> NULL

# purrr provides the walk family of functions that ignore the return values of the .f and instead return .x
walk(names, welcome)
#> Welcome Hadley!
#> Welcome Jenny!

## walk2()

tempfile() # tempfile returns a vector of character strings which can be used as names for temporary files. 
# [1] "C:\\Users\\Przemo\\AppData\\Local\\Temp\\RtmpUP9D7X\\file353810341ab"

temp <- tempfile()
dir.create(temp)

cyls <- split(mtcars, mtcars$cyl)
paths <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))
walk2(cyls, paths, write.csv)

dir(temp)
#> [1] "cyl-4.csv" "cyl-6.csv" "cyl-8.csv"

## 9.4.4 Iterating over values and indices

# imap(x, f) is equivalent to map2(x, names(x), f) if x has names, and map2(x, seq_along(x), f) if it does not.

imap_chr(iris, ~ paste0("The first value of ", .y, " is ", .x[[1]]))


x <- map(1:6, ~ sample(1000, 10))
imap_chr(x, function(x,y) paste0("The highest value of ", y, " is ", max(x)))
#> [1] "The highest value of 1 is 975" "The highest value of 2 is 915"
#> [3] "The highest value of 3 is 982" "The highest value of 4 is 955"
#> [5] "The highest value of 5 is 971" "The highest value of 6 is 696"

map2_chr(x, seq_along(x), function(x,y) paste0("The highest value of ", y, " is ", max(x)))
map2_chr(x, seq_along(x), function(inside_list, index_of_element) paste0("The highest value of ", index_of_element, " is ", max(inside_list)))

## 9.4.5 Any number of inputs: pmap() and friends

# There’s a simple equivalence between map2() and pmap(): map2(x, y, f) is the same as pmap(list(x, y), f). 
#The pmap() equivalent to the map2_dbl(xs, ws, weighted.mean) used above is:

pmap_dbl(list(xs, ws), weighted.mean)
#> [1]    NA 0.451 0.603 0.452 0.563 0.510 0.342 0.464

pmap_dbl(list(xs, ws), weighted.mean, na.rm = TRUE)
#> [1] 0.504 0.451 0.603 0.452 0.563 0.510 0.342 0.464

#  I’ve carefully chosen to match them to the arguments to runif(), so the pmap(params, runif) is equivalent to 
# runif(n = 1L, min = 0, max = 1), runif(n = 2, min = 10, max = 100), runif(n = 3L, min = 100, max = 1000)

params <- tibble::tribble(
  ~ n, ~ min, ~ max,
  1L,     0,     1,
  2L,    10,   100,
  3L,   100,  1000
)

pmap(params, runif)
#> [[1]]
#> [1] 0.332
#> 
#> [[2]]
#> [1] 53.5 47.6
#> 
#> [[3]]
#> [1] 231 715 515


### EXCERCISE ###
cyls <- split(mtcars, mtcars$cyl)
paths <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))
walk2(cyls, paths, write.csv)

temp <- tempfile()
dir.create(temp)

cyls <- split(mtcars, mtcars$cyl)
names(cyls) <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))
iwalk(cyls, ~ write.csv(.x, .y))

# Q3 Explain how the following code transforms a data frame using functions stored in a list.

trans <- list(
  disp = function(x) x * 0.0163871,
  am = function(x) factor(x, labels = c("auto", "manual"))
)

nm <- names(trans)
mtcars[nm] <- map2(trans, mtcars[nm], function(f, var) f(var))



### 9.5 Reduce family
## 9.5.1 Basics

# reduce() takes a vector of length n and produces a vector of length 1 by calling a function with a pair of
# values at a time: reduce(1:4, f) is equivalent to f(f(f(1, 2), 3), 4).

intersect(c(1:5), c(3:11)) # <- częśc wspólna 2 przedziałów
#[1] 3 4 5

l <- map(1:4, ~ sample(1:10, 15, replace = T))
str(l)
#> List of 4
#>  $ : int [1:15] 7 1 8 8 3 8 2 4 7 10 ...
#>  $ : int [1:15] 3 1 10 2 5 2 9 8 5 4 ...
#>  $ : int [1:15] 6 10 9 5 6 7 8 6 10 8 ...
#>  $ : int [1:15] 9 8 6 4 4 5 2 9 9 6 ...

(out <- l[[1]])                 # <- pierwszy element listy
(out <- intersect(out, l[[2]])) # <- częsc wspólna 1 i 2 elementu listy
(out <- intersect(out, l[[3]])) # <- częsc wspólna (1,2) i 3 elementu listy
(out <- intersect(out, l[[4]])) # <- częsc wspólna (1,2,3) i 4 elementu listy
out
#> [1] 8 4

reduce(l, intersect)

union(c(1:4), c(3:6)) # <- suma przedziałów
# [1] 1 2 3 4 5 6

reduce(l, union)
#>  [1]  7  1  8  3  2  4 10  5  9  6

simple_reduce <- function(x, f) {
  out <- x[[1]]
  for (i in seq(2, length(x))) {
    out <- f(out, x[[i]])
  }
  out
}

## 9.5.2 Accumulate

# The first reduce() variant, accumulate(), is useful for understanding how reduce works, because 
# instead of returning just the final result, it returns all the intermediate results as well:

accumulate(l, intersect)
#> [[1]]
#>  [1]  7  1  8  8  3  8  2  4  7 10 10  3  7 10 10
#> 
#> [[2]]
#> [1]  1  8  3  2  4 10
#> 
#> [[3]]

x <- c(4, 3, 10)
reduce(x, `+`)
#> [1] 17

accumulate(x, `+`)
#> [1]  4  7 17


## 9.5.3 Output types

reduce("a", `+`)
#> [1] "a"
reduce(integer(), `+`)
#> Error: `.x` is empty, and no `.init` supplied
reduce(integer(), `+`, .init = 0)
#> [1] 0

### 9.6 Predicate functionals

# some(.x, .p) returns TRUE if any element matches;
# every(.x, .p) returns TRUE if all elements match;
# none(.x, .p) returns TRUE if no element matches.
# 
# These are similar to any(map_lgl(.x, .p)), all(map_lgl(.x, .p)) and all(map_lgl(.x, negate(.p))) but they terminate early: some() returns TRUE when it sees the first TRUE, and every() and none() return FALSE when they see the first FALSE or TRUE respectively.
# 
# detect(.x, .p) returns the value of the first match; detect_index(.x, .p) returns the location of the first match.
# 
# keep(.x, .p) keeps all matching elements; discard(.x, .p) drops all matching elements.

df <- data.frame(x = 1:3, y = c("a", "b", "c"))
detect(df, is.factor)
#> NULL
detect_index(df, is.factor)
#> [1] 0

str(keep(df, is.factor))
#> 'data.frame':    3 obs. of  0 variables
str(discard(df, is.factor))
#> 'data.frame':    3 obs. of  2 variables:
#>  $ x: int  1 2 3
#>  $ y: chr  "a" "b" "c"


## 9.6.2 Map variants

df <- data.frame(
  num1 = c(0, 10, 20),
  num2 = c(5, 6, 7),
  chr1 = c("a", "b", "c"),
  stringsAsFactors = FALSE
)

str(map_if(df, is.numeric, mean))
#> List of 3
#>  $ num1: num 10
#>  $ num2: num 6
#>  $ chr1: chr [1:3] "a" "b" "c"
str(modify_if(df, is.numeric, mean))
#> 'data.frame':    3 obs. of  3 variables:
#>  $ num1: num  10 10 10
#>  $ num2: num  6 6 6
#>  $ chr1: chr  "a" "b" "c"
str(map(keep(df, is.numeric), mean))
#> List of 2
#>  $ num1: num 10
#>  $ num2: num 6

### EXERCISE ###

arg_max <- function(x, f) {
  out <- map_dbl(x, f) # stosuje funkcje do kazdego elementu listy
  max_val <- max(out) # sprawdza max wartosc wyniku
  index <- which(out == max_val) # sprawdza które elementu == maks wartosci
  x[index] # zwraca argumenty dla których funkcja osiągneła max wartość
}


arg_max <- function(x, f) {
  y <- map_dbl(x, f)
  x[y == max(y)]
}
arg_max(-10:5, function(x) x ^ 2)

bench::mark(
arg_max(-500000:500000, function(x) x ^ 2)
)

