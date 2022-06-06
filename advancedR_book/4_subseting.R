select <- matrix(ncol = 2, byrow = TRUE, c(
  1, 1,
  3, 1,
  2, 4
))

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
str(df[df$x == 2, ])

z <- factor(c("a", "b"))
z[1, drop = TRUE] # <- Drop levels of factor, if FALSE after subsetting noused levels will be preserve

### 4.3 Selecting a single element

x <- list(1:3, "a", 4:6)


var <- "cyl"
# Doesn't work - mtcars$var translated to mtcars[["var"]]
mtcars$var
#> NULL

# Instead use [[
mtcars[[var]]
#>  [1] 6 6 4 6 8 6 8 4 4 6 6 8 8 8 8 8 8 4 4 4 4 8 8 8 8 4 4 4 8 6 8 4

## 4.3.2 $

# The one important difference between $ and [[ is that $ does (left-to-right) partial matching:
x <- list(abc = 1)
x$a
#> [1] 1
x[["a"]]
#> NULL

options(warnPartialMatchDollar = TRUE)
x$a
#> Warning in x$a: partial match of 'a' to 'abc'
#> [1] 1

## indexing with `purrr` package 
x <- list(
  a = list("x", "y", "z"),
  b = list(3, 4, 5)
)

purrr::pluck(x, "a", 2)
#> [1] y

purrr::pluck(x, "c", 1)
#> NULL
purrr::chuck(x, "c", 1)
#> Error: Can't find name `c` in vector
purrr::pluck(x, "x", 1, .default = "NIE MA") # <- set default value if the indexing is OOB
#> [1] NA

# FOR S4 objects
# @ and slot()

# Excercise

# Given a linear model, e.g. mod <- lm(mpg ~ wt, data = mtcars), extract the residual 
# degrees of freedom. Extract the R squared from the model summary (summary(mod)).

mod <- lm(mpg ~ wt, data = mtcars)

mod$df.residual
#> [1] 30
mod[["df.residual"]]
#> [1] 30
summary(mod)$r.squared
#> [1] 0.753

### 4.5 Applications

## 4.5.1 Lookup tables (character subsetting) https://adv-r.hadley.nz/subsetting.html#lookup-tables

x <- c("m", "f", "u", "f", "f", "m", "m")
lookup <- c(m = "Male", f = "Female", u = NA)
lookup[x]
#>        m        f        u        f        f        m        m 
#>   "Male" "Female"       NA "Female" "Female"   "Male"   "Male"

unname(lookup[x])
#> [1] "Male"   "Female" NA       "Female" "Female" "Male"   "Male"



# 4.5.2 Matching and merging by hand (integer subsetting) https://adv-r.hadley.nz/subsetting.html#matching-merging

grades <- c(1, 2, 2, 3, 1)

info <- data.frame(
  grade = 3:1,
  desc = c("Excellent", "Good", "Poor"),
  fail = c(F, F, T)
)

id <- match(grades, info$grade)
id
#> [1] 3 2 2 1 3
info[id, ]
#>     grade      desc  fail
#> 3       1      Poor  TRUE
#> 2       2      Good FALSE
#> 2.1     2      Good FALSE
#> 1       3 Excellent FALSE
#> 3.1     1      Poor  TRUE



# 4.5.6 Removing columns from data frames (character )

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df$z <- NULL

df[setdiff(names(df), "z")]
#>   x y
#> 1 1 3
#> 2 2 2
#> 3 3 1


# 4.5.8 Boolean algebra versus sets (logical and integer ) 

x <- sample(10) < 4
which(x)
#> [1] 2 3 4

unwhich <- function(x, n) {
  out <- rep_len(FALSE, n)
  out[x] <- TRUE
  out
}
unwhich(which(x), 10)
#>  [1] FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE

(x1 <- 1:10 %% 2 == 0)
#>  [1] FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE
(x2 <- which(x1))
#> [1]  2  4  6  8 10
(y1 <- 1:10 %% 5 == 0)
#>  [1] FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE
(y2 <- which(y1))
#> [1]  5 10

# X & Y <-> intersect(x, y)
x1 & y1
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
intersect(x2, y2)
#> [1] 10

# X | Y <-> union(x, y)
x1 | y1
#>  [1] FALSE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE
union(x2, y2)
#> [1]  2  4  6  8 10  5

# X & !Y <-> setdiff(x, y)
x1 & !y1
#>  [1] FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE
setdiff(x2, y2)
#> [1] 2 4 6 8

# xor(X, Y) <-> setdiff(union(x, y), intersect(x, y))
xor(x1, y1)
#>  [1] FALSE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE
setdiff(union(x2, y2), intersect(x2, y2))
#> [1] 2 4 6 8 5